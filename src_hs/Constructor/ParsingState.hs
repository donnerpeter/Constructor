module Constructor.ParsingState where

import Data.Maybe
import Data.List
import Constructor.Constructions
import Constructor.Mite
import Constructor.Tree
import Constructor.Util
import Constructor.Variable
import Constructor.InteractionEnv
import Constructor.Issues (holderIssues)
import Data.Function (on)
import qualified Constructor.SemanticProperties as P
import qualified Constructor.LinkedSet as LS
import qualified Data.Set as Set
import qualified Data.Map as Map
import Constructor.Composition
import Control.Exception (assert)
import Control.Monad
import Control.Monad.State

data Sprout = Sprout { sLeftTree:: Tree, sHeadSide:: Side, sMites:: [Mite], sActiveSets:: [[Mite]] } deriving (Show)

stealLeftSubtrees :: Tree -> Tree -> State (Set.Set Tree, Set.Set Tree) [Sprout]
stealLeftSubtrees edgeTree rightTree = let
  stealableTrees tree = (:) tree $
    if isBranch tree && headSide tree == RightSide then stealableTrees (justRight tree) else []
  allInfos = [(lt, interactNodes $ interactionEnv lt rightTree) | lt <- stealableTrees edgeTree]
  createSprouts side infoPairs processedInfos = case infoPairs of
    [] -> return []
    (leftTree, infos):rest -> do
      pair <- get
      let processedTrees = select side fst snd pair
      if Set.member leftTree processedTrees then createSprouts side rest processedInfos
      else do
        let toProcess = filter (\info -> mergedHeadSide info == side && not (Set.member info processedInfos)) infos
            mites = xor [merged | (MergeInfo merged mhs) <- toProcess, mhs == side]
            maybeSprout = if null mites then [] else [Sprout leftTree side mites $ calcCandidateSets mites]
            plusTree = Set.insert leftTree processedTrees
        put $ select side (plusTree, snd pair) (fst pair, plusTree)
        next <- createSprouts side rest (Set.union processedInfos (Set.fromList toProcess))
        return $ maybeSprout ++ next
  in do
    leftVariants <- createSprouts LeftSide (reverse allInfos) Set.empty
    rightVariants <- createSprouts RightSide allInfos Set.empty
    return $ reverse leftVariants ++ rightVariants

growSprouts :: [Sprout] -> Tree -> [Tree]
growSprouts sprouts rightTree =
  catMaybes [createBranch mites leftTree rightTree side activeSets | Sprout leftTree side mites activeSets <- sprouts]

data ParsingState = ParsingState { lastVariants :: [Tree], history :: [ParsingState], sproutCache :: Map.Map [Mite] [Sprout] }
instance Show ParsingState where show state = show $ roots state
emptyState = ParsingState [] [] Map.empty

roots state = case lastVariants state of
  [] -> []
  tree:_ -> tree : roots (history state !! (treeWidth tree - 1))

chooseBestLastVariants :: [ParsingState] -> [Tree] -> [Tree]
chooseBestLastVariants finalHistory allVariants = {-trace (length result) -}result where
  competitors = map bestVariant $ filter isStableTree $ allVariants
  dup = findDuplicate competitors
  nodups = if isJust dup then error ("duplicate " ++ show dup) else competitors
  isStableTree tree = all (isStable . cxt) $ mites tree
  sortedVariants = sortBy (compare `on` (length . mergedRoots)) $ sortBy (compare `on` mergedUnhappyCount) $ sortBy (compare `on` mergedIssueCount) nodups
  result = head sortedVariants : metricAscending (metric $ head sortedVariants) (tail sortedVariants)

  metricAscending _ [] = []
  metricAscending prev (x:xs) = let m = metric x in
    if m > prev then x : metricAscending m xs else metricAscending prev xs

  metric tree = let edgeSizes = map treeWidth (edgeTrees RightSide tree) in (- mergedHandicapCount tree, - length (mergedRoots tree), length edgeSizes, edgeSizes)

  mergedRoots rightTree = rightTree : roots (finalHistory !! (treeWidth rightTree - 1))
  mergedUnhappyCount rightTree = sum [unhappyCount tree | tree <- mergedRoots rightTree]
  mergedIssueCount rightTree = sum [length $ issues tree | tree <- mergedRoots rightTree]
  mergedHandicapCount rightTree = sum [minimum (map handicapCount $ Constructor.Tree.allVariants tree) | tree <- mergedRoots rightTree]

allMergeVariants :: ParsingState -> Tree -> State [ParsingState] [Tree]
allMergeVariants state rightTree = do
  let rightWidth = treeWidth rightTree
  sprouts <- obtainSprouts state rightTree
  let mergedTrees = growSprouts sprouts rightTree
  let continueMerging mergedTree = allMergeVariants ((history state) !! (treeWidth mergedTree - rightWidth - 1)) mergedTree
  nested <- mapM continueMerging mergedTrees
  return $ [rightTree] ++ concat nested

obtainSprouts :: ParsingState -> Tree -> State [ParsingState] [Sprout]
obtainSprouts leftState right = do
  oldHistory <- get
  let rightCombined = _rightCombined $ interactiveMites right
      allEdgeTrees = LS.removeDups $ lastVariants leftState >>= \t -> reverse (edgeTrees RightSide t)
      (uncachedSprouts, _) = runState (mapM (\left -> stealLeftSubtrees left right) allEdgeTrees) (Set.empty, Set.empty)
      rightWidth = treeWidth right
      cachePoint = oldHistory !! (rightWidth - 1)
      cache = sproutCache cachePoint
      sprouts = Map.findWithDefault (concat uncachedSprouts) rightCombined cache
      newSprouts = Map.insert rightCombined sprouts cache
      newHistory = take (rightWidth - 1) oldHistory ++ [cachePoint { sproutCache = newSprouts }] ++ drop rightWidth oldHistory
  put newHistory
  return sprouts

addMites:: ParsingState -> [Mite] -> ParsingState
addMites state mites = result where
  (mergeVariants, updatedHistory) = runState (allMergeVariants state leaf) (state:history state)
  leaf = createLeaf mites $ calcCandidateSets mites
  lastVariants = chooseBestLastVariants updatedHistory mergeVariants
  result = ParsingState { lastVariants = lastVariants, history = updatedHistory, sproutCache = Map.empty }

calcCandidateSets:: [Mite] -> [[Mite]]
calcCandidateSets mites = {-if length result < 10 then result else trace ("---contradictors:", length mites, length result, mites) $ -}result where
  contradictorCache = Map.fromList [(m, Set.fromList $ filter (contradict m) mites) | m <- mites]
  hasContradictors mite inList = let contras = (Map.!) contradictorCache mite in any (flip Set.member contras) inList
  enumerate :: [Mite] -> [Mite] -> [Mite] -> [[Mite]]
  enumerate mites chosen uncovered =
    if any (\mite -> not $ hasContradictors mite mites) uncovered then []
    else case mites of
      [] -> assert (null uncovered) [reverse chosen]
      mite:rest -> includeMite++omitMite where
        includeMite = if contradictsChosen then [] else enumerate nextUnprocessed (mite:chosen) nextUncovered where
          contradictsChosen = any (flip Set.member contras) chosen
          nextUnprocessed = filter (not . flip Set.member contras) rest
          nextUncovered = filter (not . flip Set.member contras) uncovered
          contras = (Map.!) contradictorCache mite
        omitMite = enumerate rest chosen (mite:uncovered)
  result = enumerate mites [] []
