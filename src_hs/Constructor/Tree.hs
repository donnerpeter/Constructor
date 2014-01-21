module Constructor.Tree where

import Data.Maybe
import Data.List
import qualified Data.Set as Set
import Constructor.Constructions

data Tree = Tree {mites::[Mite], left::Maybe Tree, right::Maybe Tree, leftHeaded::Bool, baseMites::[Mite], active::Set.Set Mite} deriving (Ord, Eq)
instance Show Tree where
  show tree =
    let inner = \tree prefix ->
          let myLine = prefix ++ (Data.List.intercalate ", " $ map showMite $ headMites tree)++"\n" in
          case rightSubTree tree of
            Just r -> (inner r ("  "++prefix)) ++ myLine
            Nothing -> myLine
        allActive = allActiveMites tree
        showMite mite = (if Set.member mite allActive then "*" else "") ++ (show mite)
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

allActiveMites tree =
  if isBranch tree 
  then Set.union (active tree) $ Set.union (allActiveMites $ fromJust $ left tree) (allActiveMites $ fromJust $ right tree)
  else active tree