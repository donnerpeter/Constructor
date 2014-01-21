module Constructor.Tree where

import Data.Maybe
import Data.Function (on)
import Data.List
import Debug.Trace (traceShow)
import qualified Data.Set as Set
import qualified Constructor.LinkedSet as LS
import Constructor.Constructions
import Constructor.Lexicon
import Constructor.Composition

data Tree = Tree {mites::[Mite], left::Maybe Tree, right::Maybe Tree, leftHeaded::Bool, baseMites::[Mite]} deriving (Ord, Eq)
instance Show Tree where
  show tree =
    let inner = \tree prefix ->
          let myLine = prefix ++ (Data.List.intercalate ", " [show m | m <- headMites tree])++"\n" in
          case rightSubTree tree of
            Just r -> (inner r ("  "++prefix)) ++ myLine
            Nothing -> myLine
    in "\n" ++ inner tree ""

allTreeMites tree =
  if isNothing (left tree) then mites tree
  else (allTreeMites $ fromJust $ left tree)++(allTreeMites $ fromJust $ right tree)++(mites tree)

headMites tree =
  let inner tree suppressed =
        let ownMites = [mite | mite <- mites tree, not $ Set.member mite suppressed] in
        if isNothing $ left tree then ownMites
        else ownMites ++ (inner (if leftHeaded tree then fromJust $ left tree else fromJust $ right tree)
                                (Set.union suppressed $ Set.fromList [mite | mite <- baseMites tree, not $ happy mite]))
  in inner tree Set.empty

rightSubTree tree =
  if isNothing (left tree) then Nothing
  else if leftHeaded tree then right tree
  else Nothing
  
isBranch tree = isJust (left tree)
isDirectedBranch tree isLeftBranch = isBranch tree && leftHeaded tree == isLeftBranch

createEdges:: Tree -> Tree -> [Tree]
createEdges leftTree rightTree =
  let infos = interactNodes (headMites leftTree) (headMites rightTree)
      infos2 = infos --if null infos then infos else traceShow infos infos 
      trees = [Tree merged (Just leftTree) (Just rightTree) leftHeadedMerge base | (MergeInfo merged leftHeadedMerge base) <- infos2]
  in trees

type MergeResult = Either Tree (Tree, Tree)
integrateSubTree :: Tree -> Tree -> Bool -> MergeResult -> [MergeResult]  
integrateSubTree leftTree rightTree toLeft subResult =
  let leftLeft = fromJust $ left leftTree
      rightRight = fromJust $ right rightTree
      newEdges = \ subTree -> createEdges (if toLeft then leftLeft else subTree) (if toLeft then subTree else rightRight)
  in
  case subResult of
    Left subTree ->
      if isDirectedBranch subTree (not toLeft)
      then if toLeft then [Right (leftLeft, subTree)] else [Right (subTree, rightRight)]
      else [Left tree | tree <- newEdges subTree]
    Right (x1, x2) ->
      let subTree = if toLeft then x1 else x2
          another = if toLeft then x2 else x1
      in
      if isDirectedBranch subTree (not toLeft)
      then []
      else [if toLeft then Right (tree, another) else Right (another, tree) | tree <- newEdges subTree]
  
optimize:: Tree -> Tree -> Bool -> Bool -> Bool -> [Either Tree (Tree, Tree)]
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
addMites state mites = mergeTrees $ (Tree mites Nothing Nothing True []):state

candidateSets:: [Mite] -> [Set.Set Mite]
candidateSets mites = enumerate mites [] where
  enumerate mites chosen = case mites of
    [] -> [Set.fromList chosen]
    mite:rest -> includeMite++omitMite where
      includeMite = if hasContradictors mite chosen then [] 
                    else enumerate [r | r <- rest, not $ contradict mite r] (mite:chosen)
      omitMite = if hasContradictors mite rest then enumerate rest chosen else []

