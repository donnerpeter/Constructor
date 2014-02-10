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
  let infos = interactNodes leftTree (headMites leftTree) (headMites rightTree)
      infos2 = infos --if null infos then infos else traceShow infos infos 
      trees = [Tree merged (Just leftTree) (Just rightTree) leftHeadedMerge Set.empty $ calcCandidateSets merged | (MergeInfo merged leftHeadedMerge) <- infos2]
  in catMaybes $ map suggestActive trees

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
addMites state mites = mergeTrees $ (fromJust $ suggestActive $ Tree mites Nothing Nothing True Set.empty $ calcCandidateSets mites):state

calcCandidateSets:: [Mite] -> [Set.Set Mite]
calcCandidateSets mites = {-traceShow ("calcCandidates", mites, "\ncontradictors:", contradictorCache, length result) $ -}result where
  contradictorCache = Map.fromList [(m, Set.fromList $ filter (contradict m) mites) | m <- mites]
  contradictCached m1 m2 = Set.member m2 $ (Map.!) contradictorCache m1
  hasContradictors mite inList = let contras = (Map.!) contradictorCache mite in any (flip Set.member contras) inList
  enumerate :: [Mite] -> [Mite] -> [Mite] -> [Set.Set Mite]
  enumerate mites chosen uncovered =
    if any (\mite -> not $ hasContradictors mite mites) uncovered then [] 
    else case mites of
      [] -> assert (null uncovered) [Set.fromList chosen]
      mite:rest -> includeMite++omitMite where
        includeMite = if hasContradictors mite chosen then [] 
                      else enumerate (filter (not . contradictCached mite) rest) (mite:chosen) (filter (not . contradictCached mite) uncovered)
        omitMite = enumerate rest chosen (mite:uncovered)
  result = enumerate mites [] []

suggestActive:: Tree -> Maybe Tree
suggestActive tree = {-traceShow ("suggestActive", tree, "->", result) $ -}result where
  result = inner tree True True True Set.empty 
  inner tree leftBorder rightBorder borderHead spine = {-traceShow ("inner", spine, mites tree, tree) $-} do
    let candidates = [set | set <- candidateSets tree, all (flip Set.member set) requiredMites]
        requiredMites = filter (flip Set.member spine) (mites tree)
        unhappyCount set = length $ filter (\mite -> not (happy mite || Set.member mite spine)) (Set.elems set)
        absolutelyHappy = [set | set <- candidates, unhappyCount set == 0]
        anyBorder = leftBorder || rightBorder || borderHead
    singleCandidate <- listToMaybe $ if anyBorder then Data.List.sortBy (compare `on` unhappyCount) candidates else absolutelyHappy 
    if isBranch tree then do
      let nextSpine = Set.union spine (Set.fromList $ activeBase singleCandidate)
      newLeft <- inner (fromJust $ left tree) leftBorder False (leftHeaded tree && anyBorder) nextSpine
      newRight <- inner (fromJust $ right tree) False rightBorder ((not $ leftHeaded tree) && anyBorder) nextSpine
      Just $ tree { active = singleCandidate, left = Just newLeft, right = Just newRight }
    else Just $ tree { active = singleCandidate }
