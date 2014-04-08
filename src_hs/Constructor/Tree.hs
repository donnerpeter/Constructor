module Constructor.Tree where

import Data.Maybe
import Data.List
import Data.Function (on)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Constructor.LinkedSet as LS
import Constructor.Mite
import Constructor.Util
import Constructor.Issues

data Side = LeftSide | RightSide deriving (Eq, Show, Ord)
invert LeftSide = RightSide
invert RightSide = LeftSide
select LeftSide x _ = x
select RightSide _ x = x

data Tree = Tree {
  mites::[Mite], left::Maybe Tree, right::Maybe Tree, headSide::Side,
  active::Set.Set Mite, allActiveMiteList :: [Mite], allActiveMiteSet :: Set.Set Mite,
  activeHeadMites :: [Mite],
  _uncoveredActiveMites :: Set.Set Mite,
  _unhappyLeft :: [Mite], _unhappyRight :: [Mite], _unhappyHead :: [Mite],
  _issues :: [Issue],
  allVariants:: [Tree]
}

instance Show Tree where
  show tree =
    let inner tree prefix allowTop allowBottom = top ++ center ++ bottom where
          center = prefix ++ (Data.List.intercalate ", " $ map showMite $ uncoveredHeadMites tree) ++ "\n"
          top = if not allowTop then "" else case listToMaybe $ subTrees RightSide tree of
            Just r -> inner r (".."++prefix) True False
            Nothing -> ""
          bottom = if not allowBottom then "" else case listToMaybe $ subTrees LeftSide tree of
            Just r -> inner r (".."++prefix) False True
            Nothing -> ""
          showMite mite =
            let shown = show mite
                patched = if Set.member mite spine && "!" `isPrefixOf` shown then drop 1 shown else shown
            in (if Set.member mite allActive then "*" else "") ++ patched
        allActive = allActiveMiteSet tree
        spine = activeBase allActive
    in "\n" ++ inner tree "." True True

justLeft tree = fromJust $ left tree
justRight tree = fromJust $ right tree

allTreeMites tree =
  if isNothing $ left tree then mites tree
  else (allTreeMites $ fromJust $ left tree)++mites tree++(allTreeMites $ fromJust $ right tree)

subTrees side tree =
  if not $ isBranch tree then []
  else if headSide tree == LeftSide && side == RightSide then justRight tree : subTrees side (justLeft tree)
  else if headSide tree == RightSide && side == LeftSide then justLeft tree : subTrees side (justRight tree)
  else subTrees side $ (if side == LeftSide then justLeft else justRight) tree

headTrees tree =
  if isBranch tree then tree:headTrees (if headSide tree == LeftSide then justLeft tree else justRight tree)
  else [tree]

headMites tree = concat $ map mites $ headTrees tree

uncoveredHeadMites tree =
  let inner tree suppressed result =
        let ownMites = [mite | mite <- mites tree, not $ Set.member mite suppressed]
        in
        if isNothing $ left tree then concat $ reverse (ownMites:result)
        else inner (if headSide tree == LeftSide then justLeft tree else justRight tree)
                    (Set.union suppressed $ Set.filter (not . happy) $ activeBase $ active tree)
                    (ownMites:result)
  in inner tree Set.empty []

activeBase activeSet = Set.fromList [mite | activeMite <- Set.elems activeSet, mite <- baseMites activeMite]

isBranch tree = isJust (left tree)

unhappyActiveMites tree = result where
  allActive = allActiveMiteSet tree
  spine = activeBase allActive
  result = filter (\mite -> not (happy mite || Set.member mite spine)) $ Set.elems allActive

createLeaf mites candidateSets = head trees where
  trees = sortAVs $ map eachLeaf candidateSets
  eachLeaf active = let activeSet = Set.fromList active in Tree {
      mites = mites, left = Nothing, right = Nothing, headSide = LeftSide,
      active = activeSet, allActiveMiteList = active, allActiveMiteSet = activeSet, _uncoveredActiveMites = activeSet,
      activeHeadMites = active,
      _unhappyLeft = [], _unhappyRight = [], _unhappyHead = filter (not. happy) active,
      _issues = issues active,
      allVariants = trees
    }

createBranch mites _leftChild _rightChild headSide candidateSets = listToMaybe allBranchVariants where
  allBranchVariants = sortAVs $ map leastUnhappy $ Map.elems grouped
  leftAVs = filter (null . _unhappyRight) (allVariants _leftChild)
  rightAVs = filter (null . _unhappyLeft) (allVariants _rightChild)
  allAVCandidates = {-traceShow ("-------------------leftAVs", leftAVs) $ -}do
    active <- candidateSets
    let covered = base active
        base mites = LS.removeDups [mite | activeMite <- mites, mite <- baseMites activeMite]
        isUncovered mite = not $ mite `elem` covered
        unhappyBase = Set.fromList $ filter (not . happy) covered
        isCompatible av = not $ any (flip Set.member unhappyBase) $ base $ activeHeadMites av
    aLeft <- {-trace ("----------------active", length activeSets, active) $ -}leftAVs
    if not (isCompatible aLeft) then [] else do
    let missingInLeft = filter (not . flip Set.member (allActiveMiteSet aLeft)) covered
    aRight <- rightAVs
    if not (isCompatible aRight) then [] else do
    let missingInRight = filter (not . flip Set.member (allActiveMiteSet aRight)) missingInLeft
    if {-trace ("-----------checkRight", active) $ traceShow ("missingInRight", missingInRight) $ -}null missingInRight
    then
      let childrenActive = {-trace ("ok", active) $ -}Set.union (_uncoveredActiveMites aLeft) (_uncoveredActiveMites aRight)
          activeSet = Set.fromList active
          headChild = select headSide aLeft aRight
          allActiveList = allActiveMiteList aLeft ++ active ++ allActiveMiteList aRight
      in
      return $ Tree {
        mites = mites, left = Just aLeft, right = Just aRight, headSide = headSide,
        active = activeSet,
        allActiveMiteList = allActiveList,
        allActiveMiteSet = Set.union activeSet $ Set.union (allActiveMiteSet aLeft) (allActiveMiteSet $ aRight),
        activeHeadMites = active ++ filter (\mite -> isUncovered mite || happy mite) (activeHeadMites headChild),
        _uncoveredActiveMites = Set.union (Set.filter (\mite -> isUncovered mite || happy mite) childrenActive) activeSet,
        _unhappyLeft  = filter isUncovered $ _unhappyLeft aLeft   ++ select headSide [] (_unhappyHead aLeft),
        _unhappyRight = filter isUncovered $ _unhappyRight aRight ++ select headSide (_unhappyHead aRight) [],
        _unhappyHead = filter isUncovered $ _unhappyHead headChild ++ filter (not. happy) active,
        _issues = issues allActiveList,
        allVariants = allBranchVariants
      }
    else []
  grouped = Map.fromListWith (++) [(activeHeadMites av, [av]) | av <- allAVCandidates]
  leastUnhappy avs = head $ sortAVs avs

treeWidth tree = if isBranch tree then treeWidth (justLeft tree) + treeWidth (justRight tree) else 1

sortAVs avs = Data.List.sortBy (compare `on` (\av -> (unhappyCount av, avIssueCount av))) avs where
  unhappyCount av = length (_unhappyLeft av) + length (_unhappyHead av) + length (_unhappyRight av)
  avIssueCount av = length $ _issues av