module Constructor.Tree where

import Data.Maybe
import Data.List
import Constructor.Constructions
import Constructor.Lexicon
import Constructor.Composition

data Tree = Tree {mites::[Mite], left::Maybe Tree, right::Maybe Tree, leftHeaded::Bool}
instance Show Tree where
  show tree =
    let inner = \tree prefix ->
          let myLine = prefix ++ (Data.List.intercalate ", " [show m | m <- headMites tree])++"\n" in
          case rightSubTree tree of
            Just r -> (inner r (" "++prefix)) ++ myLine
            Nothing -> myLine
    in "\n" ++ inner tree ""

headMites tree =
  if isNothing (left tree) then mites tree
  else (mites tree)++(headMites $ fromJust $ (if leftHeaded tree then left tree else right tree))

rightSubTree tree =
  if isNothing (left tree) then Nothing
  else if leftHeaded tree then right tree
  else Nothing
  
isBranch tree = isJust (left tree)
isDirectedBranch tree isLeftBranch = isBranch tree && leftHeaded tree == isLeftBranch

createEdges leftTree rightTree leftMites rightMites =
  let mergeInfos = concat [interactMites (cxt leftMite) (cxt rightMite) | leftMite <- leftMites, rightMite <- rightMites] in
  [Tree mergedMites (Just leftTree) (Just rightTree) leftHeadedMerge | MergeInfo mergedMites _ leftHeadedMerge <- mergeInfos]

integrateSubTree leftTree rightTree toLeft subResult =
  let leftLeft = fromJust $ left leftTree
      rightRight = fromJust $ right rightTree
      newEdges = \ subTree -> createEdges leftLeft subTree 
                              (headMites (if toLeft then leftLeft else subTree)) 
                              (headMites (if toLeft then subTree else rightRight))
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
                   then [Left tree | tree <- createEdges leftTree rightTree (headMites leftTree) (headMites rightTree)]
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
mergeTrees state@(rightTree:leftTree:rest) =
  let treeCandidates = optimize leftTree rightTree True True True
      singleCandidates = filter (\ x -> case x of Left _ -> True; Right _ -> False) treeCandidates
  in case singleCandidates of
    Left first:_ -> mergeTrees (first:rest)
    _ -> state
mergeTrees trees = trees

addMites:: [Tree] -> [Mite] -> [Tree]
addMites state mites = mergeTrees $ (Tree mites Nothing Nothing True):state
