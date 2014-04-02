module Constructor.ParsingState where

import Data.Maybe
import Data.List
import Constructor.Mite
import Constructor.Tree
import Constructor.Util
import Data.Function (on)
import qualified Constructor.LinkedSet as LS
import qualified Data.Set as Set
import qualified Data.Map as Map
import Constructor.Composition
import Control.Exception (assert)

createEdges leftTree rightTree infos = --trace infos $
  let lh = xor [merged | (MergeInfo merged side) <- infos, side == LeftSide]
      rh = xor [merged | (MergeInfo merged side) <- infos, side == RightSide]
      lTree = if null lh then Nothing else createBranch lh leftTree rightTree LeftSide $ calcCandidateSets lh
      rTree = if null rh then Nothing else createBranch rh leftTree rightTree RightSide $ calcCandidateSets rh
  in catMaybes [lTree, rTree]

calcMergeInfos leftTree rightTree = infos where
  leftMites = LS.removeDups $ foldl (++) [] $ map activeHeadMites $ map (flip applyAV leftTree) $ avs leftTree
  rightMites = LS.removeDups $ foldl (++) [] $ map activeHeadMites $ map (flip applyAV rightTree) $ avs rightTree
  infos = interactNodes leftTree leftMites rightMites

stealLeft:: Tree -> Tree -> Set.Set MergeInfo -> [Tree]
stealLeft leftTree rightTree processedInfos =
  let ownResults = createEdges leftTree rightTree $ filter (not. flip Set.member processedInfos) infos
      infos = if null (avUnhappyRight $ head $ avs leftTree) && null (avUnhappyLeft $ head $ avs rightTree)
              then calcMergeInfos leftTree rightTree
              else []
      nextProcessed = if headSide leftTree == LeftSide then Set.empty else Set.union processedInfos (Set.fromList infos)
      leftSubResults = if isBranch leftTree then stealLeft (justRight leftTree) rightTree nextProcessed else []
  in leftSubResults ++ ownResults

data ParsingState = ParsingState { roots :: [Tree], history :: [ParsingState] }
instance Show ParsingState where show state = show $ roots state
emptyState = ParsingState [] []

mergeTrees:: ParsingState -> ParsingState
mergeTrees state = {-trace ("--------------", length allVariants, [(sortingKey v, v) | v <- allVariants]) $ -}result where
  result = head $ Data.List.sortBy (compare `on` sortingKey) $ allVariants
  sortingKey state = (length $ roots state, unhappyCount state, stateIssueCount state)
  unhappyCount state = sum [length $ unhappyActiveMites tree | tree <- roots state]
  allVariants = tryStealing (roots state)
  tryStealing :: [Tree] -> [ParsingState]
  tryStealing trees = [state { roots = trees }] ++ case trees of
    right:left:rest -> let
      stolen = stealLeft left right Set.empty
      prevState tree = (history state) !! (treeWidth tree - 1)
      nextStates = map (\tree -> tree : roots (prevState tree)) stolen
      in nextStates >>= tryStealing
    _ -> []

addMites:: ParsingState -> [Mite] -> ParsingState
addMites state mites = mergeTrees $ ParsingState { roots = (createLeaf mites $ calcCandidateSets mites):roots state, history = state:history state }

calcCandidateSets:: [Mite] -> [[Mite]]
calcCandidateSets mites = {-trace ("---contradictors:", length mites, length result, mites) $ -}result where
  contradictorCache = buildContradictorCache mites
  hasContradictors mite inList = let contras = (Map.!) contradictorCache mite in any (flip Set.member contras) inList
  enumerate :: [Mite] -> [Mite] -> [Mite] -> [[Mite]]
  enumerate mites chosen uncovered =
    if any (\mite -> not $ hasContradictors mite mites) uncovered then []
    else case mites of
      [] -> assert (null uncovered) [chosen]
      mite:rest -> includeMite++omitMite where
        includeMite = if contradictsChosen then [] else enumerate nextUnprocessed (mite:chosen) nextUncovered where
          contradictsChosen = any (flip Set.member contras) chosen
          nextUnprocessed = filter (not . flip Set.member contras) rest
          nextUncovered = filter (not . flip Set.member contras) uncovered
          contras = (Map.!) contradictorCache mite
        omitMite = enumerate rest chosen (mite:uncovered)
  result = enumerate mites [] []

stateIssueCount state = sum [length $ avIssues $ head $ avs tree | tree <- roots state]

activeStateMites trees = concat (map allActiveMites $ reverse trees)