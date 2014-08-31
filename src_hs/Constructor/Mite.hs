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
  flattenBaseKeys :: [XorKey],
  flattenContradictors :: Set.Set XorKey,
  xorKey :: XorKey
}

_initMite cxt _contradictors baseMites = let
  mite = Mite {
      cxt = cxt, happy = isHappy cxt, contradictors = _contradictors, baseMites = baseMites,
      xorKey = (cxt, baseMites),
      flattenBaseMites = fbm,
      flattenBaseKeys = map xorKey fbm,
      flattenContradictors = foldl Set.union Set.empty (map contradictors fbm)
    }
  fbm = mite : (LS.removeDups (baseMites >>= flattenBaseMites))
  in
  if contradict mite mite then error $ "self-contradicting mite " ++ show mite ++ "; flattenContradictors=" ++ show (flattenContradictors mite)
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
      createMite key@(c, b) = _initMite c (Set.union (contras key cxt2ExistingContras) (contras key cxt2Contras)) b
      newMites = map createMite allCxts
  in assert (LS.removeDups newMites == newMites) $ {-traceShow ("xor", miteGroups) $ traceShow ("->", newMites) $ -}newMites

contradict mite1 mite2 = any (flip Set.member (flattenContradictors mite1)) $ flattenBaseKeys mite2

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
