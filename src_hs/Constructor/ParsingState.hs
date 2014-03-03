module Constructor.ParsingState where

import Data.Maybe
import Data.List
import Constructor.Constructions
import Constructor.Tree
import Constructor.Util
import Data.Function (on)
import qualified Constructor.LinkedSet as LS
import qualified Data.Set as Set
import qualified Data.Map as Map
import Constructor.Composition
import Control.Exception (assert)

createEdges:: Tree -> Tree -> [Tree]
createEdges leftTree rightTree =
  let leftMites = LS.removeDups $ foldl (++) [] $ map activeHeadMites $ map (flip applyAV leftTree) $ avs leftTree
      rightMites = LS.removeDups $ foldl (++) [] $ map activeHeadMites $ map (flip applyAV rightTree) $ avs rightTree
      infos = interactNodes leftTree leftMites rightMites
      lh = xor [merged | (MergeInfo merged leftHeadedMerge) <- infos, leftHeadedMerge]
      rh = xor [merged | (MergeInfo merged leftHeadedMerge) <- infos, not leftHeadedMerge]
      lTree = if null lh then Nothing else createBranch lh leftTree rightTree LeftSide $ calcCandidateSets lh
      rTree = if null rh then Nothing else createBranch rh leftTree rightTree RightSide $ calcCandidateSets rh
      trees = catMaybes [lTree, rTree]
  in trees

data MergeResult = Single Tree | Couple Tree Tree deriving (Show)
integrateSubTree :: Tree -> Side -> MergeResult -> [MergeResult]
integrateSubTree orphan side subResult = --trace ("---integrateSubTree", activeHeadMites orphan) $
  let handlePair t1 t2 digLeft digRight = [Couple t1 t2] ++ (map Single $ mergeTwoTrees t1 t2 digLeft digRight)
  in case subResult of
    Single subTree -> select side (handlePair orphan subTree True False) (handlePair subTree orphan False True)
    Couple x1 x2 -> concat [select side (handlePair adoption x2 False False) (handlePair x1 adoption False False) |
                            adoption <- select side (mergeTwoTrees orphan x1 True False) (mergeTwoTrees x2 orphan False True)]

mergeTwoTrees leftTree rightTree digLeft digRight = concat $ map toTrees $ optimize leftTree rightTree digLeft digRight True where
  toTrees result = case result of
    Single x -> [x]
    _ -> []

optimize:: Tree -> Tree -> Bool -> Bool -> Bool -> [MergeResult]
optimize leftTree rightTree digLeft digRight useOwnMites =
  let ownResults = if useOwnMites && null (avUnhappyRight $ head $ avs leftTree) && null (avUnhappyLeft $ head $ avs rightTree)
                   then [Single tree | tree <- createEdges leftTree rightTree]
                   else []
      leftSubResults = if digLeft && isBranch leftTree
                       then optimize (justRight leftTree) rightTree True False $ headSide leftTree == LeftSide
                       else []
      rightSubResults = if digRight && isBranch rightTree
                        then optimize leftTree (justLeft rightTree) False True $ headSide rightTree == RightSide
                        else []
      dugLeft = concat $ map (integrateSubTree (justLeft leftTree) LeftSide) leftSubResults
      dugRight = concat $ map (integrateSubTree (justRight rightTree) RightSide) rightSubResults
      in ownResults ++ dugLeft ++ dugRight

mergeTrees:: [Tree] -> [Tree]
mergeTrees state =
  head $ Data.List.sortBy (compare `on` (\state -> (length state, unhappyCount state, stateIssueCount state))) $ allMergeVariants [state] LS.empty where
    unhappyCount trees = sum [length $ unhappyActiveMites tree | tree <- trees]
    allMergeVariants queue result =
      case queue of
        [] -> LS.elements result
        state:restQueue ->
          allMergeVariants (restQueue++notConsideredMerges) $ LS.addAll (state:notConsideredMerges) result where
            immediateMerges = case state of
              rightTree:leftTree:restTrees -> map toTrees $ optimize leftTree rightTree True True True where
                toTrees result = case result of
                  Single x -> x:restTrees
                  Couple x y -> y:x:restTrees
              _ -> []
            notConsideredMerges = [m | m <- immediateMerges, not $ LS.member m result]

addMites:: [Tree] -> [Mite] -> [Tree]
addMites state mites = mergeTrees $ (createLeaf mites $ calcCandidateSets mites):state

calcCandidateSets:: [Mite] -> [[Mite]]
calcCandidateSets mites = {-traceShow ("contradictors:", mites) $ -}result where
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

stateIssueCount trees = sum [length $ avIssues $ head $ avs tree | tree <- trees]

activeStateMites trees = concat (map allActiveMites $ reverse trees)