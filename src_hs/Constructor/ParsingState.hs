module Constructor.ParsingState where

import Data.Maybe
import Data.List
import Constructor.Constructions
import Constructor.Tree
import Debug.Trace
import Data.Function (on)
import qualified Constructor.LinkedSet as LS
import qualified Data.Set as Set
import Constructor.Composition

createEdges:: Tree -> Tree -> [Tree]
createEdges leftTree rightTree =
  let infos = interactNodes (headMites leftTree) (headMites rightTree)
      infos2 = infos --if null infos then infos else traceShow infos infos 
      trees = [Tree merged (Just leftTree) (Just rightTree) leftHeadedMerge Set.empty | (MergeInfo merged leftHeadedMerge) <- infos2]
  in catMaybes $ map suggestActive trees

type MergeResult = Either Tree (Tree, Tree)
integrateSubTree :: Tree -> Tree -> Bool -> MergeResult -> [MergeResult]  
integrateSubTree leftTree rightTree toLeft subResult =
  let leftLeft = fromJust $ left leftTree
      rightRight = fromJust $ right rightTree
      newEdges = \ subTree -> createEdges (if toLeft then leftLeft else subTree) (if toLeft then subTree else rightRight)
      handlePair t1 t2 = [Right (t1, t2)] ++ (map Left $ createEdges t1 t2)
  in
  case subResult of
    Left subTree ->
      if isDirectedBranch subTree (not toLeft)
      then if toLeft then handlePair leftLeft subTree else handlePair subTree rightRight
      else [Left tree | tree <- newEdges subTree]
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
  head $ sortByLength $ allMergeVariants [state] LS.empty where
    sortByLength = Data.List.sortBy (compare `on` length)
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
addMites state mites = mergeTrees $ (fromJust $ suggestActive $ Tree mites Nothing Nothing True Set.empty):state

candidateSets:: [Mite] -> [Set.Set Mite]
candidateSets mites = enumerate mites [] where
  enumerate :: [Mite] -> [Mite] -> [Set.Set Mite]
  enumerate mites chosen = case mites of
    [] -> [Set.fromList chosen]
    mite:rest -> includeMite++omitMite where
      includeMite = if hasContradictors mite chosen then [] 
                    else enumerate [r | r <- rest, not $ contradict mite r] (mite:chosen)
      omitMite = if hasContradictors mite rest then enumerate rest chosen else []

suggestActive:: Tree -> Maybe Tree
suggestActive tree = {-traceShow (tree, "->", result) $ -}result where
  result = inner tree True True True Set.empty 
  inner tree leftBorder rightBorder borderHead spine = do
    let candidates = [set | set <- candidateSets (mites tree), all (flip Set.member set) requiredMites]
        requiredMites = filter (flip Set.member spine) (mites tree)
        absolutelyHappy = [set | set <- candidates, all (\mite -> happy mite || Set.member mite spine) (Set.elems set)]
        anyBorder = leftBorder || rightBorder || borderHead
    singleCandidate <- listToMaybe $ if anyBorder then candidates else absolutelyHappy 
    if isBranch tree then do
      let nextSpine = Set.union spine (Set.fromList $ activeBase singleCandidate)
      newLeft <- inner (fromJust $ left tree) leftBorder False (leftHeaded tree && anyBorder) nextSpine
      newRight <- inner (fromJust $ right tree) False rightBorder ((not $ leftHeaded tree) && anyBorder) nextSpine
      Just $ tree { active = singleCandidate, left = Just newLeft, right = Just newRight }
    else Just $ tree { active = singleCandidate }
