module Constructor.ParsingState where

import Data.Maybe
import Data.List
import Constructor.Constructions
import Constructor.Tree
import Debug.Trace
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

type MergeResult = Either Tree (Tree, Tree)
integrateSubTree :: Tree -> Tree -> Bool -> MergeResult -> [MergeResult]  
integrateSubTree leftTree rightTree toLeft subResult = -- traceShow ("integrateSubTree", leftTree, rightTree, subResult) $
  let leftLeft = fromJust $ left leftTree
      rightRight = fromJust $ right rightTree
      newEdges = \ subTree -> createEdges (if toLeft then leftLeft else subTree) (if toLeft then subTree else rightRight)
      handlePair t1 t2 = [Right (t1, t2)] ++ (map Left $ createEdges t1 t2)
  in
  case subResult of
    Left subTree ->
      if toLeft then handlePair leftLeft subTree else handlePair subTree rightRight
    Right (x1, x2) ->
      let subTree = if toLeft then x1 else x2
          another = if toLeft then x2 else x1
      in
      if isDirectedBranch subTree (not toLeft)
      then []
      else concat [if toLeft then handlePair tree another else handlePair another tree | tree <- newEdges subTree]
  
optimize:: Tree -> Tree -> Bool -> Bool -> Bool -> [MergeResult]
optimize leftTree rightTree digLeft digRight useOwnMites =
  let ownResults = if useOwnMites 
                   then [Left tree | tree <- createEdges leftTree rightTree]
                   else []
      leftSubResults = if digLeft && isBranch leftTree
                       then optimize (fromJust $ right leftTree) rightTree True False $ isDirectedBranch leftTree True 
                       else []
      rightSubResults = if digRight && isBranch rightTree 
                        then optimize leftTree (fromJust $ left rightTree) False True $ isDirectedBranch rightTree False
                        else []
      dugLeft = concat $ map (integrateSubTree leftTree rightTree True) leftSubResults
      dugRight = concat $ map (integrateSubTree leftTree rightTree False) rightSubResults
      in ownResults ++ dugLeft ++ dugRight

mergeTrees:: [Tree] -> [Tree]
mergeTrees state =
  head $ sortByLength $ sortByUnhappy $ allMergeVariants [state] LS.empty where
    sortByLength = Data.List.sortBy (compare `on` length)
    sortByUnhappy = Data.List.sortBy (compare `on` unhappyCount)
    unhappyCount trees = sum [length $ unhappyActiveMites tree | tree <- trees]
    allMergeVariants queue result =
      case queue of
        [] -> LS.elements result
        state:restQueue ->
          allMergeVariants (restQueue++notConsideredMerges) $ LS.addAll (state:notConsideredMerges) result where
            immediateMerges = case state of
              rightTree:leftTree:restTrees -> map toTrees $ optimize leftTree rightTree True True True where
                toTrees result = case result of
                  Left x -> x:restTrees
                  Right (x, y) -> y:x:restTrees
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
