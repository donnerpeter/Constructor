module Constructor.Tree where

import Data.Maybe
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Constructor.LinkedSet as LS
import Constructor.Mite
import Constructor.Util
import Constructor.Issues
import Constructor.Sense (makeSense, composeSense, Sense, Fact(..))
import Constructor.Constructions (Construction(Sem, Unify))

data Tree = Tree {
  mites::[Mite], left::Maybe Tree, right::Maybe Tree, headSide::Side,
  active::Set.Set Mite, allActiveMiteSet :: Set.Set Mite,
  activeHeadMites :: [Mite],
  activeHeadMitesBase :: [Mite],
  _uncoveredActiveMites :: Set.Set Mite,
  _unhappyLeft :: [Mite], _unhappyRight :: [Mite], _unhappyHead :: [Mite],
  _issues :: [Issue],
  allVariants:: [Tree],
  sense :: Sense,
  bestVariant :: Tree
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

-- | Returns all trees adjacent to this one from the specified side
subTrees side tree =
  if not $ isBranch tree then []
  else if headSide tree == LeftSide && side == RightSide then justRight tree : subTrees side (justLeft tree)
  else if headSide tree == RightSide && side == LeftSide then justLeft tree : subTrees side (justRight tree)
  else subTrees side $ (if side == LeftSide then justLeft else justRight) tree

-- | top-level visible trees from the specified side
edgeTrees side tree = (:) tree $ case subTrees side tree of
  child:_ -> edgeTrees side child
  [] -> []

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
                    (Set.union suppressed $ Set.filter isCoverable $ activeBase $ active tree)
                    (ownMites:result)
  in inner tree Set.empty []

activeBase activeSet = Set.fromList [mite | activeMite <- Set.elems activeSet, mite <- baseMites activeMite]

isBranch tree = isJust (left tree)

unhappyCount tree = length (_unhappyLeft tree) + length (_unhappyHead tree) + length (_unhappyRight tree)

nodeSense active = makeSense facts unifications where
  facts = [Fact var value | (cxt -> Sem var value) <- active]
  unifications = [(var1, var2) | (cxt -> Unify var1 var2) <- active]

createLeaf mites candidateSets = head trees where
  trees = map eachLeaf candidateSets
  best = bestTree trees
  eachLeaf active = let
    activeSet = Set.fromList active
    _sense = nodeSense active
    in Tree {
      mites = mites, left = Nothing, right = Nothing, headSide = LeftSide,
      active = activeSet, allActiveMiteSet = activeSet, _uncoveredActiveMites = activeSet,
      activeHeadMites = active,
      activeHeadMitesBase = LS.removeDups (active >>= baseMites),
      _unhappyLeft = [], _unhappyRight = [], _unhappyHead = filter (not. happy) active,
      _issues = issues _sense,
      allVariants = trees,
      sense = _sense,
      bestVariant = best
    }

createBranch mites _leftChild _rightChild headSide candidateSets = listToMaybe allBranchVariants where
  best = bestTree allBranchVariants
  allBranchVariants = do
    active <- candidateSets
    let covered = base active
        base mites = LS.removeDups [mite | activeMite <- mites, mite <- baseMites activeMite]
        isUncovered mite = not $ mite `elem` covered
        coverableBase = Set.fromList $ filter isCoverable covered
        isCompatible av = not $ any (flip Set.member coverableBase) $ activeHeadMitesBase av
        leftCompatible  = filter isCompatible $ filter (null . _unhappyRight) $ allVariants _leftChild
        rightCompatible = filter isCompatible $ filter (null . _unhappyLeft)  $ allVariants _rightChild
        sideCompatible = select headSide rightCompatible leftCompatible
    headChild <- select headSide leftCompatible rightCompatible
    if (not $ null $ filter isUncovered $ (select headSide _unhappyRight _unhappyLeft) headChild) then []
    else
      let uncoveredByHeadChild = filter (not . flip Set.member (allActiveMiteSet headChild)) covered
      in case branchCandidates active headChild sideCompatible covered uncoveredByHeadChild of
        [] -> []
        candidates -> [bestTree candidates]
  branchCandidates active headChild sideCompatible activeBase uncoveredByHeadChild = let
    allVariants = catMaybes $ map createCandidate sideCompatible
    isUncovered mite = not $ mite `elem` activeBase
    createCandidate sideChild = let
      uncoveredByBothChildren = filter (not . flip Set.member (allActiveMiteSet sideChild)) uncoveredByHeadChild
      childrenActive = Set.union (_uncoveredActiveMites headChild) (_uncoveredActiveMites sideChild)
      activeSet = Set.fromList active
      aLeft = select headSide headChild sideChild
      aRight = select headSide sideChild headChild
      _activeHeadMites = active ++ filter (\mite -> isUncovered mite || not (isCoverable mite)) (activeHeadMites headChild)
      _sense = sense aLeft `composeSense` nodeSense active `composeSense` sense aRight
      in
      if null uncoveredByBothChildren
      then Just $ Tree {
          mites = mites, left = Just aLeft, right = Just aRight, headSide = headSide,
          active = activeSet,
          allActiveMiteSet = Set.union activeSet $ Set.union (allActiveMiteSet aLeft) (allActiveMiteSet aRight),
          activeHeadMites = _activeHeadMites,
          activeHeadMitesBase = LS.removeDups (_activeHeadMites >>= baseMites),
          _uncoveredActiveMites = Set.union (Set.filter (\mite -> isUncovered mite || not (isCoverable mite)) childrenActive) activeSet,
          _unhappyLeft  = filter isUncovered $ _unhappyLeft aLeft   ++ select headSide [] (_unhappyHead aLeft),
          _unhappyRight = filter isUncovered $ _unhappyRight aRight ++ select headSide (_unhappyHead aRight) [],
          _unhappyHead = filter isUncovered $ _unhappyHead headChild ++ filter (not. happy) active,
          _issues = issues _sense,
          allVariants = allBranchVariants,
          sense = _sense,
          bestVariant = best
        }
      else Nothing
    in allVariants

treeWidth tree = if isBranch tree then treeWidth (justLeft tree) + treeWidth (justRight tree) else 1

bestTree avs = head $ leastValued (length . _issues) $ leastValued unhappyCount avs