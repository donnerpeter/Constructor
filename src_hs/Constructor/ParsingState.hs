module Constructor.ParsingState where

import Data.Maybe
import Data.List
import Constructor.Constructions (isStable)
import Constructor.Mite
import Constructor.Tree
import Constructor.Util
import Data.Function (on)
import qualified Constructor.LinkedSet as LS
import qualified Data.Set as Set
import qualified Data.Map as Map
import Constructor.Composition
import Control.Exception (assert)

calcMergeInfos leftTree rightMites = infos where
  leftMites = LS.removeDups $ foldl (++) [] $ map activeHeadMites $ allVariants leftTree
  infos = interactNodes leftTree leftMites rightMites

stealLeft leftTree rightTree = let
  stealableTrees tree = (:) tree $
    if isBranch tree && headSide tree == RightSide then stealableTrees (justRight tree) else []
  rightMites = LS.removeDups $ foldl (++) [] $ map activeHeadMites $ allVariants rightTree
  stealFromHead tree = let
    allInfos = [(lt, calcMergeInfos lt rightMites) | lt <- stealableTrees tree]
    createBranches side infoPairs processedInfos = case infoPairs of
      [] -> []
      (leftTree, infos):rest -> let
        toProcess = filter (\info -> mergedHeadSide info == side && not (Set.member info processedInfos)) infos
        mites = xor [merged | (MergeInfo merged mhs) <- toProcess, mhs == side]
        maybeBranch = if null mites then Nothing else createBranch mites leftTree rightTree side $ calcCandidateSets mites
        in maybeToList maybeBranch ++ createBranches side rest (Set.union processedInfos (Set.fromList toProcess))
    in (reverse $ createBranches LeftSide (reverse allInfos) Set.empty) ++ createBranches RightSide allInfos Set.empty
  in reverse (edgeTrees RightSide leftTree) >>= stealFromHead

data ParsingState = ParsingState { roots :: [Tree], history :: [ParsingState] }
instance Show ParsingState where show state = show $ roots state
emptyState = ParsingState [] []

mergeTrees:: ParsingState -> ParsingState
mergeTrees state = {-trace ("--------------", length allVariants, [(sortingKey v, v) | v <- allVariants]) $ -}result where
  result = head $ leastValued stateIssueCount $ leastValued unhappyCount $ leastValued (length . roots) allVariants
  unhappyCount state = sum [length $ unhappyActiveMites tree | tree <- roots state]
  isStableState state = all isStable $ map cxt $ (roots state >>= mites)
  allVariants = filter isStableState $ tryStealing (roots state)
  tryStealing :: [Tree] -> [ParsingState]
  tryStealing trees = [state { roots = trees }] ++ case trees of
    right:left:rest -> let
      stolen = stealLeft left right
      prevState tree = (history state) !! (treeWidth tree - 1)
      nextStates = map (\tree -> tree : roots (prevState tree)) stolen
      in nextStates >>= tryStealing
    _ -> []

addMites:: ParsingState -> [Mite] -> ParsingState
addMites state mites = mergeTrees $ ParsingState { roots = (createLeaf mites $ calcCandidateSets mites):roots state, history = state:history state }

calcCandidateSets:: [Mite] -> [[Mite]]
calcCandidateSets mites = {-trace ("---contradictors:", length mites, length result, mites) $ -}result where
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

activeStateMites trees = concat (map allActiveMiteList $ reverse trees)