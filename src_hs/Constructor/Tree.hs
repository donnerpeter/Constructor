module Constructor.Tree where

import Data.Maybe
import Data.List
import Data.Function (on)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Constructor.LinkedSet as LS
import Constructor.Mite
import Constructor.Sense
import Constructor.Util

data Side = LeftSide | RightSide deriving (Eq, Show, Ord)
invert LeftSide = RightSide
invert RightSide = LeftSide
select LeftSide x _ = x
select RightSide _ x = x

data Tree = Tree {mites::[Mite], left::Maybe Tree, right::Maybe Tree, headSide::Side, active::Set.Set Mite, avs:: [ActiveVariant], allActiveMiteSet :: Set.Set Mite}

cmpKey tree = (headSide tree, mites tree, active tree, left tree, right tree)

instance Eq Tree where t1 == t2 = cmpKey t1 == cmpKey t2
instance Ord Tree where compare t1 t2 = compare (cmpKey t1) (cmpKey t2)
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

activeHeadMites tree = filter (flip Set.member allActive) (uncoveredHeadMites tree) where
  allActive = foldl Set.union Set.empty $ map active $ headTrees tree

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

allActiveMites tree = filter (flip Set.member activeSet) (allTreeMites tree) where activeSet = allActiveMiteSet tree

unhappyActiveMites tree = result where
  allActive = allActiveMiteSet tree
  spine = activeBase allActive
  result = filter (\mite -> not (happy mite || Set.member mite spine)) $ Set.elems allActive

data ActiveVariant = ActiveVariant { avMites :: [Mite], avLeft :: Maybe Tree, avRight :: Maybe Tree,
  avAllActive :: Set.Set Mite,
  avUnhappyLeft :: [Mite], avUnhappyHead :: [Mite], avUnhappyRight :: [Mite],
  avIssues :: [Issue]
  } deriving (Show)
type Issue = String

createLeaf mites candidateSets = let avs = leafAVs candidateSets in
  applyAV (head avs) $ Tree mites Nothing Nothing LeftSide Set.empty avs Set.empty

createBranch mites leftChild rightChild headSide candidateSets = let avs = branchAVs leftChild rightChild headSide candidateSets in
  case avs of
    [] -> Nothing
    av:_ -> Just $ applyAV av $ Tree mites (Just leftChild) (Just rightChild) headSide Set.empty avs Set.empty

applyAV av tree = let activeSet = Set.fromList $ avMites av in
  tree { active = activeSet, left = avLeft av, right = avRight av,
         allActiveMiteSet =
           if isBranch tree
           then Set.union activeSet $ Set.union (allActiveMiteSet $ fromJust $ avLeft av) (allActiveMiteSet $ fromJust $ avRight av)
           else activeSet
       }

leafAVs :: [[Mite]] -> [ActiveVariant]
leafAVs activeSets = sortAVs $ map (\active ->
  ActiveVariant active Nothing Nothing (Set.fromList active) [] (filter (not. happy) active) [] (issues active))
  activeSets

branchAVs :: Tree -> Tree -> Side -> [[Mite]] -> [ActiveVariant]
branchAVs leftChild rightChild headSide activeSets = {-traceShow ("------------------activeSets", activeSets) $ -}let
  leftAVs = filter (null . avUnhappyRight) (avs leftChild)
  rightAVs = filter (null . avUnhappyLeft) (avs rightChild)
  allAVCandidates = {-traceShow ("-------------------leftAVs", leftAVs) $ -}do
    active <- activeSets
    let covered = base active
        base mites = LS.removeDups [mite | activeMite <- mites, mite <- baseMites activeMite]
        isUncovered mite = not $ mite `elem` covered
        unhappyBase = Set.fromList $ filter (not . happy) covered
        isCompatible av = not $ any (flip Set.member unhappyBase) $ base $ activeHeadMites av
    aLeft <- {-trace ("----------------active", length activeSets, active) $ -}leftAVs
    if not (isCompatible aLeft) then [] else do
    let leftChildCandidate = applyAV aLeft leftChild
    let missingInLeft = filter (not . flip Set.member (allActiveMiteSet leftChildCandidate)) covered
    aRight <- rightAVs
    if not (isCompatible aRight) then [] else do
    let rightChildCandidate = applyAV aRight rightChild
    let missingInRight = filter (not . flip Set.member (allActiveMiteSet rightChildCandidate)) missingInLeft
    if {-trace ("-----------checkRight", active) $ traceShow ("missingInRight", missingInRight) $ -}null missingInRight
    then
      let childrenActive = {-trace ("ok", active) $ -}Set.union (avAllActive aLeft) (avAllActive aRight) in
      return $ ActiveVariant {
        avMites = active,
        avLeft = Just leftChildCandidate,
        avRight = Just rightChildCandidate,
        avAllActive = Set.union (Set.filter (\mite -> isUncovered mite || happy mite) childrenActive) $ Set.fromList active,
        avUnhappyLeft = filter isUncovered $ avUnhappyLeft aLeft ++ (if headSide == LeftSide then [] else avUnhappyHead aLeft),
        avUnhappyHead = filter isUncovered $ avUnhappyHead (if headSide == LeftSide then aLeft else aRight) ++ filter (not. happy) active,
        avUnhappyRight = filter isUncovered $ avUnhappyRight aRight ++ (if headSide == RightSide then [] else avUnhappyHead aRight),
        avIssues = issues $ allActiveMites leftChildCandidate ++ active ++ allActiveMites rightChildCandidate
      }
    else []
  grouped = Map.fromListWith (++) [(activeHeadMites av, [av]) | av <- allAVCandidates]
  result = sortAVs $ map leastUnhappy $ Map.elems grouped
  activeHeadMites av = filter (flip Set.member (avAllActive av)) inHead where
    inHead = avMites av ++ headMites (if headSide == LeftSide then leftChild else rightChild)
  leastUnhappy avs = head $ sortAVs avs
  in
  {-trace ("branchAVs", result) $ -}result

treeWidth tree = if isBranch tree then treeWidth (justLeft tree) + treeWidth (justRight tree) else 1

sortAVs avs = Data.List.sortBy (compare `on` (\av -> (unhappyCount av, avIssueCount av))) avs where
  unhappyCount av = length (avUnhappyLeft av) + length (avUnhappyHead av) + length (avUnhappyRight av)
  avIssueCount av = length $ avIssues av

issues :: [Mite] -> [Issue]
issues mites = let
  sense = makeSense mites
  frames = allFrames sense
  hasCP = any (hasAnyType ["fact", "question"]) frames
  frameIssues frame = case getType frame of
    Just "seq" | Nothing == sValue "conj" frame -> ["comma-only seq"]
    Just "SIT" -> if Nothing == (fValue "arg1" frame >>= sDeclaredValue "type") then ["unknown sit subj "] else []
    Just "ASK" -> if any (hasType "fact") (flatten $ fValue "topic" frame) then ["asking fact"] else []
    Just "THEY" -> if isJust $ fValue "relative" frame then ["relative clause for pronoun"] else []
    Just "WE" -> if isJust $ fValue "relative" frame then ["relative clause for pronoun"] else []
    Just "COME_SCALARLY" -> case fValue "arg1" frame of
      Just subj ->
        if Nothing == sDeclaredValue "type" subj then ["unknown subj"] else
         case fValue "order" frame of
          Just order | earlier frame "order" subj "type" && earlier frame "type" frame "order" -> ["come_scalarly order subj"]
          _ -> []
    _ -> []
  in {-traceIt "issues" $ -}(frames >>= frameIssues) ++ (if hasCP then [] else ["no clause"])