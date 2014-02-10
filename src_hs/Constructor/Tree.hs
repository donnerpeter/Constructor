module Constructor.Tree where

import Data.Maybe
import Data.List
import Debug.Trace
import qualified Data.Set as Set
import Constructor.Constructions

data Side = LeftSide | RightSide deriving (Eq, Show)

data Tree = Tree {mites::[Mite], left::Maybe Tree, right::Maybe Tree, leftHeaded::Bool, active::Set.Set Mite, candidateSets:: [Set.Set Mite]} deriving (Ord, Eq)
instance Show Tree where
  show tree =
    let inner tree prefix allowTop allowBottom = top ++ center ++ bottom where
          center = prefix ++ (Data.List.intercalate ", " $ map showMite $ headMites tree) ++ "\n"
          top = if not allowTop then "" else case listToMaybe $ subTrees RightSide tree of
            Just r -> inner r ("  "++prefix) True False
            Nothing -> ""
          bottom = if not allowBottom then "" else case listToMaybe $ subTrees LeftSide tree of
            Just r -> inner r ("  "++prefix) False True
            Nothing -> ""
          showMite mite =
            let shown = show mite
                patched = if Set.member mite spine && "!" `isPrefixOf` shown then drop 1 shown else shown
            in (if Set.member mite allActive then "*" else "") ++ patched
        allActive = allActiveMiteSet tree
        spine = Set.fromList $ activeBase allActive
    in "\n" ++ inner tree "" True True

allTreeMites tree =
  if isNothing $ left tree then mites tree
  else (allTreeMites $ fromJust $ left tree)++mites tree++(allTreeMites $ fromJust $ right tree)

subTrees side tree =
  if not $ isBranch tree then []
  else if leftHeaded tree && side == RightSide then fromJust (right tree) : subTrees side (fromJust $ left tree)
  else if not (leftHeaded tree) && side == LeftSide then fromJust (left tree) : subTrees side (fromJust $ right tree)
  else subTrees side $ fromJust $ (if side == LeftSide then left else right) tree

headMites tree =
  let inner tree suppressed result =
        let ownMites = [mite | mite <- mites tree, not $ Set.member mite suppressed, compatible mite]
            compatible mite = all (not . contradictsAll mite) result
            contradictsAll mite layer = (not $ null layer) && all (contradict mite) layer
        in
        if isNothing $ left tree then concat $ reverse (ownMites:result)
        else inner (if leftHeaded tree then fromJust $ left tree else fromJust $ right tree)
                    (Set.union suppressed $ Set.fromList [mite | mite <- activeBase $ active tree, not $ happy mite])
                    (ownMites:result)
  in inner tree Set.empty []

activeBase activeSet = [mite | activeMite <- Set.elems activeSet, mite <- baseMites activeMite]  

isBranch tree = isJust (left tree)
isDirectedBranch tree isLeftBranch = isBranch tree && leftHeaded tree == isLeftBranch

allActiveMiteSet tree =
  if isBranch tree 
  then Set.union (active tree) $ Set.union (allActiveMiteSet $ fromJust $ left tree) (allActiveMiteSet $ fromJust $ right tree)
  else active tree

allActiveMites tree = filter (flip Set.member activeSet) (allTreeMites tree) where activeSet = allActiveMiteSet tree

unhappyActiveMites tree = filter (\mite -> not (happy mite || Set.member mite spine)) $ Set.elems allActive where
  allActive = allActiveMiteSet tree
  spine = Set.fromList $ activeBase $ allActive
