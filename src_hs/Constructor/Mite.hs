module Constructor.Mite
 (Mite(Mite, cxt, happy, baseMites), mite,
 xor, withBase, contradict,
 optional,
 isCoverable,
 semS, semV, semT)
 where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Constructor.LinkedSet as LS
import Data.List
import Control.Exception (assert)
import Constructor.Constructions
import Constructor.Variable
import Constructor.SemanticProperties

type XorKey = (Construction, [Mite])

data Mite = Mite {
  cxt :: Construction,
  happy :: Bool,
  contradictors :: Set.Set XorKey,
  baseMites :: [Mite],
  flattenBaseMites :: [Mite],
  flattenContradictors :: Set.Set XorKey,
  xorKey :: XorKey
}

_initMite cxt _contradictors baseMites = let
  mite = Mite {
      cxt = cxt, happy = isHappy cxt, contradictors = _contradictors, baseMites = baseMites,
      xorKey = (cxt, baseMites),
      flattenBaseMites = fbm,
      flattenContradictors = foldl Set.union Set.empty (map contradictors fbm)
    }
  fbm = mite : (LS.removeDups (baseMites >>= flattenBaseMites))
  in
  if contradict mite mite then error $ "self-contradicting mite " ++ show mite ++ "\nbase=" ++ show baseMites ++ "\nflattenContradictors=" ++ show (flattenContradictors mite)
  else mite

instance Show Mite where
  show (Mite {cxt=c, happy=h, contradictors=cc, baseMites = b}) =
    (if h then "" else "!") ++ show c -- ++ (if Set.null cc then "" else "(xor "++(show cc)++")")
        -- ++ (if null b then "" else "(base " ++ show b ++ "/base)")
instance Ord Mite where compare m1 m2 = compare (xorKey m1) (xorKey m2)
instance Eq Mite where m1 == m2 = (xorKey m1) == (xorKey m2)
  
mite cxt = _initMite cxt Set.empty []
  
semS var prop value = mite $ Sem var (StrValue prop value)
semV var prop value = mite $ Sem var (VarValue prop value)
semT var _type = semS var Type _type

xor :: [[Mite]] -> [Mite]
xor _miteGroups = let
      miteGroups = diversify $ LS.removeDups _miteGroups
      cxtGroups = map (map xorKey) miteGroups
      allCxts = LS.removeDups $ concat cxtGroups
      allCxtSet = Set.fromList allCxts

      cxt2ExistingContras :: Map.Map XorKey (Set.Set XorKey)
      cxt2ExistingContras = Map.fromListWith Set.union [(xorKey mite, contradictors mite) | mite <- concat miteGroups]
      cxt2Friends = Map.fromListWith Set.union $ [(c, Set.fromList group) | group <- cxtGroups, c <- group]
      cxt2Contras = Map.map (\friends -> Set.difference allCxtSet friends) cxt2Friends

      contras :: XorKey -> Map.Map XorKey (Set.Set XorKey) -> Set.Set XorKey
      contras c fromMap = Map.findWithDefault Set.empty c fromMap
      createMite key@(c, b) = _initMite c (Set.union (contras key cxt2ExistingContras) (contras key cxt2Contras)) b
      newMites = map createMite allCxts

  in
    if any null miteGroups then error $ "Empty mite group: " ++ show miteGroups
    else assert (LS.removeDups newMites == newMites) $ {-traceShow ("xor", miteGroups) $ traceShow ("->", newMites) $ -}newMites

contradict mite1 mite2 = any (\m -> Set.member (xorKey m) (flattenContradictors mite1)) $ flattenBaseMites mite2

diversify miteGroups = result where
  cxt2Count = Map.fromListWith (+) [(cxt mite, 1) | mite <- concat miteGroups]
  isUnique mite = 1 == Map.findWithDefault 0 (cxt mite) cxt2Count
  toProcess = filter isToProcess miteGroups
  isToProcess group = all (not . isUnique) group
  diversifyGroup g = case elemIndex g toProcess of
    Just i -> g ++ [mite $ Diversifier i]
    _ -> g
  result = map diversifyGroup miteGroups

withBase base mites = let
  keys = Set.fromList $ map xorKey mites
  addBaseToContra key@(c, b) = if Set.member key keys then (c, LS.removeDups $ b ++ base) else key
  handleMite m = let
    newBase = LS.removeDups $ (baseMites m ++ base)
    issues = concat [if contradict m1 m2 then [(m1, m2)] else [] | m1 <- newBase, m2 <- newBase]
    prettyIssues = intercalate "\n" $ map (\(m1, m2) -> show m1 ++ "\n  contradicts\n  " ++ show m2) issues
    in
    if null issues
    then _initMite (cxt m) (Set.map addBaseToContra $ contradictors m) newBase
    else error $ "contradictory base for mite " ++ show m ++ ":\n" ++ prettyIssues ++ "\nallBase=" ++ show newBase
  in map handleMite mites

optional mites = xor [mites, [mite $ EmptyCxt $ cxt $ head mites]]

isCoverable mite = if not (happy mite) then True else case cxt mite of
  NomHead {} -> True
  _ -> False
