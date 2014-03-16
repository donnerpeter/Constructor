module Constructor.Mite where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Constructor.LinkedSet as LS
import Control.Exception (assert)
import Constructor.Constructions
import Constructor.Variable

type XorKey = (Construction, [Mite])
data Mite = Mite { cxt :: Construction, happy :: Bool, contradictors :: Set.Set XorKey, baseMites :: [Mite] }
instance Show Mite where
  show (Mite {cxt=c, happy=h, contradictors=cc, baseMites = b}) =
    (if h then "" else "!") ++ show c -- ++ (if Set.null cc then "" else "(xor "++(show cc)++")")
        -- ++ (if null b then "" else "(base " ++ show b ++ "/base)")
instance Ord Mite where compare m1 m2 = compare (xorKey m1) (xorKey m2)
instance Eq Mite where m1 == m2 = (xorKey m1) == (xorKey m2)
  
mite cxt = Mite cxt (isHappy cxt) Set.empty []
  
semS var prop value = mite $ Sem var prop (StrValue value)
semV var prop value = mite $ Sem var prop (VarValue value)
semT var _type = semS var "type" _type

xorKey mite = (cxt mite, baseMites mite)

xor :: [[Mite]] -> [Mite]
xor miteGroups =
  let cxtGroups = map (map xorKey) miteGroups
      allCxts = LS.removeDups $ concat cxtGroups
      allCxtSet = Set.fromList allCxts

      cxt2ExistingContras :: Map.Map XorKey (Set.Set XorKey)
      cxt2ExistingContras = Map.fromListWith Set.union [(xorKey mite, contradictors mite) | mite <- concat miteGroups]
      cxt2Friends = Map.fromListWith Set.union $ [(c, Set.fromList group) | group <- cxtGroups, c <- group]
      cxt2Contras = Map.map (\friends -> Set.difference allCxtSet friends) cxt2Friends

      contras :: XorKey -> Map.Map XorKey (Set.Set XorKey) -> Set.Set XorKey
      contras c fromMap = Map.findWithDefault Set.empty c fromMap
      createMite key@(c, b) = (mite c) { contradictors = Set.union (contras key cxt2ExistingContras) (contras key cxt2Contras), baseMites = b}
      newMites = map createMite allCxts
  in assert (LS.removeDups newMites == newMites) $ {-traceShow ("xor", miteGroups) $ traceShow ("->", newMites) $ -}newMites
  
flattenBaseMites m = m:(baseMites m >>= flattenBaseMites)
flattenContradictors mite = foldl Set.union Set.empty (map contradictors $ flattenBaseMites mite) where

contradict mite1 mite2 = let allContradictors1 = flattenContradictors mite1 in
  any (flip Set.member allContradictors1) $ map xorKey $ flattenBaseMites mite2

buildContradictorCache mites = Map.fromList [(m, Set.fromList $ findContradictors t) | t@(m, _, _) <- triples] where
  triples = [(m, LS.removeDups $ map xorKey $ flattenBaseMites m, flattenContradictors m) | m <- mites]
  findContradictors (mite, _, allContradictors) = [m | (m, _, _) <- filter contradicts triples] where
    contradicts (m2, flatBase2, _) = any (flip Set.member allContradictors) flatBase2

withBase base mites = let
  keys = Set.fromList $ map xorKey mites
  addBaseToContra key@(c, b) = if Set.member key keys then (c, LS.removeDups $ b ++ base) else key
  handleMite m = let
    newBase = LS.removeDups $ (baseMites m ++ base)
    issues = concat [if contradict m1 m2 then [(m1, m2)] else [] | m1 <- newBase, m2 <- newBase]
    in
    if null issues
    then m {baseMites = newBase, contradictors = Set.map addBaseToContra $ contradictors m }
    else error $ "contradictory base for mite " ++ show m ++ ": " ++ show issues ++ "\nallBase=" ++ show newBase
  in map handleMite mites

optional mites = xor [mites, [mite $ EmptyCxt $ cxt $ head mites]]