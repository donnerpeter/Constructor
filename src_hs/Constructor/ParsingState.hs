module Constructor.ParsingState where

import Data.Maybe
import Data.List
import Constructor.Constructions (isStable, isLastResort)
import Constructor.Mite
import Constructor.Tree
import Constructor.Util
import Constructor.InteractionEnv
import Data.Function (on)
import qualified Constructor.LinkedSet as LS
import qualified Data.Set as Set
import qualified Data.Map as Map
import Constructor.Composition
import Control.Exception (assert)
import Control.Monad
import Control.Monad.State

data Sprout = Sprout { sLeftTree:: Tree, sHeadSide:: Side, sMites:: [Mite], sActiveSets:: [[Mite]] } deriving (Show)

stealLeftSubtrees :: Tree -> [[Mite]] -> [Mite] -> [Sprout]
stealLeftSubtrees leftTree rightSets rightCombined = let
  stealableTrees tree = (:) tree $
    if isBranch tree && headSide tree == RightSide then stealableTrees (justRight tree) else []
  stealFromHead tree = let
    allInfos = [(lt, interactNodes $ interactionEnv lt rightSets rightCombined) | lt <- stealableTrees tree]
    createSprouts side infoPairs processedInfos = case infoPairs of
      [] -> []
      (leftTree, infos):rest -> let
        toProcess = filter (\info -> mergedHeadSide info == side && not (Set.member info processedInfos)) infos
        mites = xor [merged | (MergeInfo merged mhs) <- toProcess, mhs == side]
        maybeSprout = if null mites then [] else [Sprout leftTree side mites $ calcCandidateSets mites]
        in maybeSprout ++ createSprouts side rest (Set.union processedInfos (Set.fromList toProcess))
    in (reverse $ createSprouts LeftSide (reverse allInfos) Set.empty) ++ createSprouts RightSide allInfos Set.empty
  in reverse (edgeTrees RightSide leftTree) >>= stealFromHead

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
chooseBestLastVariants finalHistory allVariants = {-if length result > 1 then trace (length result) result else -}result where
  competitors = map bestVariant $ filter isStableTree allVariants
  isStableTree tree = all (isStable . cxt) $ mites tree
  sortedVariants = sortBy (compare `on` (length . mergedRoots)) $ sortBy (compare `on` mergedUnhappyCount) $ sortBy (compare `on` mergedIssueCount) competitors
  hasLastResort tree = any (isLastResort . cxt) $ Set.elems $ allActiveMiteSet tree
  result = head sortedVariants : lastResortDescending (lastResortCount $ head sortedVariants) (tail sortedVariants)

  lastResortDescending _ [] = []
  lastResortDescending maxLR (x:xs) = let lrc = lastResortCount x in
    if lrc < maxLR then x : lastResortDescending lrc xs else lastResortDescending maxLR xs

  lastResortCount rightTree = sum [length $ filter (isLastResort . cxt) $ Set.elems $ allActiveMiteSet tree | tree <- mergedRoots rightTree]
  mergedRoots rightTree = rightTree : roots (finalHistory !! (treeWidth rightTree - 1))
  mergedUnhappyCount rightTree = sum [unhappyCount tree | tree <- mergedRoots rightTree]
  mergedIssueCount rightTree = sum [length $ _issues tree | tree <- mergedRoots rightTree]

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
  let rightSets = map activeHeadMites $ Constructor.Tree.allVariants right
      rightCombined = LS.removeDups $ concat rightSets
      uncachedSprouts = lastVariants leftState >>= \left -> stealLeftSubtrees left rightSets rightCombined
      rightWidth = treeWidth right
      cachePoint = oldHistory !! (rightWidth - 1)
      cache = sproutCache cachePoint
      sprouts = Map.findWithDefault uncachedSprouts rightCombined cache
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
