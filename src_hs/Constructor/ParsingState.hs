module Constructor.ParsingState where

import Data.Maybe
import Data.List
import Constructor.Constructions (isStable)
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
  tree:_ -> tree:roots ((history state) !! (treeWidth tree - 1))

mergeTrees:: ParsingState -> ParsingState
mergeTrees state = {-trace ("--------------", length allVariants, [(sortingKey v, v) | v <- allVariants]) $ -}result where
  chooseBestRoots state = state { lastVariants = map bestVariant $ lastVariants state }
  result = head $ leastValued stateIssueCount $ leastValued stateUnhappyCount $ map chooseBestRoots $ leastValued (length . roots) allVariants
  stateUnhappyCount state = sum [unhappyCount tree | tree <- roots state]
  isStableState state = all isStable $ map cxt $ (roots state >>= mites)
  allVariants = [state { history = updatedHistory } | state <- stateRecombinations, isStableState state]
  (stateRecombinations, updatedHistory) = runState (tryStealing state) (history state)
  tryStealing :: ParsingState -> State [ParsingState] [ParsingState]
  tryStealing state = case roots state of
    right:left:rest -> do
      oldHistory <- get
      let rightSets = map activeHeadMites $ Constructor.Tree.allVariants right
          rightCombined = LS.removeDups $ concat rightSets
          uncachedSprouts = stealLeftSubtrees left rightSets rightCombined
          rightWidth = treeWidth right
          cachePoint = oldHistory !! (rightWidth - 1)
          cache = sproutCache cachePoint
          sprouts = Map.findWithDefault uncachedSprouts rightCombined cache
          newSprouts = Map.insert rightCombined sprouts cache
          newHistory = take (rightWidth - 1) oldHistory ++ [cachePoint { sproutCache = newSprouts }] ++ drop rightWidth oldHistory
      put newHistory
      let nextStates = map (\tree -> state { lastVariants = [tree] }) $ growSprouts sprouts right
      nested <- mapM tryStealing nextStates
      return $ [state] ++ concat nested
    _ -> return [state]

addMites:: ParsingState -> [Mite] -> ParsingState
addMites state mites = mergeTrees $ ParsingState { lastVariants = [createLeaf mites $ calcCandidateSets mites],
                                                   history = state:history state,
                                                   sproutCache = Map.empty }

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

stateIssueCount state = sum [length $ _issues tree | tree <- roots state]