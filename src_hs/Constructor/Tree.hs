module Constructor.Tree (Tree(..),
                         allTreeMites, isBranch, justRight,
                         createBranch, createLeaf,
                         subTrees, edgeTrees,
                         bestVariant, issues, sense) where

import Data.Maybe
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Constructor.LinkedSet as LS
import Control.Monad.State
import Control.Monad.List
import Constructor.Mite
import Constructor.Util
import Constructor.Issues
import Constructor.Interner
import Constructor.Sense (makeSense, composeSense, Sense, Fact(..))
import Constructor.Constructions (Construction(Sem, Unify))

data Tree = Tree {
  mites::[Mite], left::Maybe Tree, right::Maybe Tree, headSide::Side,
  active::Set.Set Mite, allActiveMiteSet :: Set.Set Mite,
  activeHeadMites :: [Mite],
  activeHeadMitesBase :: [Mite],
  _unhappy :: Unhappy,
  _issues :: Interned IssueHolder,
  allVariants:: [Tree],
  unhappyCount :: Int,
  handicapCount :: Int,
  treeWidth :: Int
}

instance Eq Tree where t1 == t2 = eqKey t1 == eqKey t2
instance Ord Tree where t1 `compare` t2 = eqKey t1 `compare` eqKey t2
eqKey t = (headSide t, mites t, left t, right t)

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

obtainHolder :: (Ord k, Show k) => k -> IssueHolder -> State (Interner k IssueHolder) (Interned IssueHolder)
obtainHolder key defaultValue = do
  interner <- get
  let (result, updated) = intern interner key defaultValue
  put updated
  return result

leafHolders :: [[Mite]] -> [([Mite], Interned IssueHolder)]
leafHolders candidateSets = pairs where
  (pairs, i) = runState (mapM each candidateSets) emptyInterner
  each active = let
    facts = [Fact var value | (cxt -> Sem var value) <- active]
    unifications = [(var1, var2) | (cxt -> Unify var1 var2) <- active]
    key = (Set.fromList facts, Set.fromList unifications)
    in do
      result <- obtainHolder key (leafHolder $ makeSense facts unifications)
      return (active, result)

createLeaf mites candidateSets = head trees where
  trees = map eachLeaf $ leafHolders candidateSets
  eachLeaf (active, issues) = let
    activeSet = Set.fromList active
    unhappy = Unhappy [] [] $ filter (not. happy) active
    in Tree {
      mites = mites, left = Nothing, right = Nothing, headSide = LeftSide,
      active = activeSet, allActiveMiteSet = activeSet,
      activeHeadMites = active,
      activeHeadMitesBase = LS.removeDups (active >>= baseMites),
      _unhappy = unhappy, unhappyCount = _unhappyCount unhappy, handicapCount = length $ filter isHandicap active,
      _issues = issues,
      allVariants = trees,
      treeWidth = 1
    }

createBranch mites _leftChild _rightChild headSide candidateSets = listToMaybe allBranchVariants where
  allBranchVariants :: [Tree]
  (allBranchVariants, i) = runState (runListT computation) emptyInterner
  computation :: ListT (State (Interner [Int] IssueHolder)) Tree
  computation = do
    let lh = leafHolders candidateSets
    (active, nodeHolder) <- ListT . return $ lh
    let covered = base active
        base mites = LS.removeDups [mite | activeMite <- mites, mite <- baseMites activeMite]
        isUncovered mite = not $ mite `elem` covered
        coverableBase = Set.fromList $ filter isCoverable covered
        isCompatible av = not $ any (flip Set.member coverableBase) $ activeHeadMitesBase av
        leftCompatible  = filter isCompatible $ filter (null . _unhappyRight . _unhappy) $ allVariants _leftChild
        rightCompatible = filter isCompatible $ filter (null . _unhappyLeft  . _unhappy) $ allVariants _rightChild
    headChild <- ListT . return $ select headSide leftCompatible rightCompatible
    if (not $ null $ filter isUncovered $ (select headSide _unhappyRight _unhappyLeft) $ _unhappy headChild) then ListT $ return []
    else do
      let
        uncoveredByHeadChild = filter (not . flip Set.member (allActiveMiteSet headChild)) covered
        checkSideChild sideChild = null $ filter (not . flip Set.member (allActiveMiteSet sideChild)) uncoveredByHeadChild
        sideChildren = filter checkSideChild $ select headSide rightCompatible leftCompatible
        _activeHeadMites = active ++ filter (\mite -> isUncovered mite || not (isCoverable mite)) (activeHeadMites headChild)
        createCandidate :: Tree -> State (Interner [Int] IssueHolder) BranchCandidate
        createCandidate sideChild = do
          let aLeft =  select headSide headChild sideChild
              aRight = select headSide sideChild headChild
              childIssues = [_issues aLeft, nodeHolder, _issues aRight]
          compositeHolder <- obtainHolder (map internedKey childIssues) $ composeHolders $ map internedValue childIssues
          return $ BranchCandidate {
              bcLeft = aLeft, bcRight = aRight,
              bcUnhappy = composeUnhappy (_unhappy aLeft) (_unhappy aRight) headSide active isUncovered,
              bcIssues = compositeHolder
            }
      ListT $ do
        candidates <- mapM createCandidate sideChildren
        let filtered = filter (null . fatalIssues . internedValue . bcIssues) candidates
        return $ if null filtered then [] else [candidatesToBranch mites headSide active _activeHeadMites allBranchVariants filtered]

data BranchCandidate = BranchCandidate { bcLeft:: Tree, bcRight:: Tree, bcIssues:: Interned IssueHolder, bcUnhappy:: Unhappy }

candidatesToBranch mites headSide active _activeHeadMites allBranchVariants candidates = let
  unhappyCount = minimum $ map (_unhappyCount . bcUnhappy) candidates
  bc = head $ leastValued (length . holderIssues . internedValue . bcIssues) $ filter (\c -> unhappyCount == _unhappyCount (bcUnhappy c)) candidates
  aLeft = bcLeft bc
  aRight = bcRight bc
  activeSet = Set.fromList active
  in Tree {
    mites = mites, left = Just aLeft, right = Just aRight, headSide = headSide, active = activeSet,
    allActiveMiteSet = Set.union activeSet $ Set.union (allActiveMiteSet aLeft) (allActiveMiteSet aRight),
    activeHeadMites = _activeHeadMites,
    activeHeadMitesBase = LS.removeDups (_activeHeadMites >>= baseMites),
    _unhappy = bcUnhappy bc, unhappyCount = unhappyCount,
    handicapCount = handicapCount aLeft + handicapCount aRight + Set.size (Set.filter isHandicap activeSet),
    _issues = bcIssues bc,
    treeWidth = treeWidth aLeft + treeWidth aRight,
    allVariants = allBranchVariants
  }

data Unhappy = Unhappy { _unhappyLeft :: [Mite], _unhappyRight :: [Mite], _unhappyHead :: [Mite] }

composeUnhappy left right headSide active isUncovered = Unhappy {
  _unhappyLeft  = filter isUncovered $ _unhappyLeft left     ++ select headSide [] (_unhappyHead left),
  _unhappyRight = filter isUncovered $ _unhappyRight right   ++ select headSide (_unhappyHead right) [],
  _unhappyHead =  filter isUncovered $ _unhappyHead (select headSide left right) ++ filter (not. happy) active
 }

sense tree = holderSense $ internedValue $ _issues tree

issues tree = holderIssues $ internedValue $ _issues tree

_unhappyCount u = length (_unhappyLeft u) + length (_unhappyHead u) + length (_unhappyRight u)

bestTree avs = head $ leastValued (length . issues) $ leastValued unhappyCount avs

bestVariant tree = bestTree $ allVariants tree