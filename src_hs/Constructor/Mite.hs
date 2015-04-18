{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Constructor.Mite
 (Mite(Mite, cxt, happy, baseMites), mite,
 xor, xorNonEmpty, withBase, contradict,
 optional,
 isCoverable, isHandicap, isInteractive,
 semS, semV, semT,
 --constructions
 Construction(..), SeqData(..), ArgKind(..), Satisfied(..), Optionality(..), SemArgKind(..),
 cases, isStable, getCommaSurroundableVar)
 where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Constructor.LinkedSet as LS
import Data.List
import Data.Maybe
import Control.Exception (assert)
import Constructor.Constructions
import Constructor.Variable
import Constructor.Util
import Constructor.SemanticProperties

type XorKey = (Construction, [Mite])

data Mite = Mite {
  cxt :: Construction,
  happy :: Bool,
  contradictors :: Set.Set XorKey,
  baseMites :: [Mite],
  flattenBaseMites :: [Mite],
  flattenContradictors :: Set.Set XorKey,
  xorKey :: XorKey,
  generations :: Map.Map Mite Int
}

_initMite cxt _contradictors baseMites = let
  mite = Mite {
      cxt = cxt, happy = isHappy cxt, contradictors = _contradictors, baseMites = baseMites,
      xorKey = (cxt, baseMites),
      flattenBaseMites = fbm,
      flattenContradictors = foldl Set.union Set.empty (map contradictors fbm),
      generations = Map.fromList $ [(ancestor, generation ancestor) | ancestor <- fbm]
    }
  fbm = mite : (LS.removeDups (baseMites >>= flattenBaseMites))
  generation ancestor = if ancestor == mite then 0 else let
    baseMaybes = map (\base -> Map.lookup ancestor (generations base)) baseMites
    in 1 + minimum (catMaybes baseMaybes)
  in
  if contradict mite mite then error $ "self-contradicting mite " ++ show mite ++ "\nbase=" ++ show baseMites ++ "\nflattenContradictors=" ++ show (flattenContradictors mite)
  else mite

instance Show Mite where
  show m =
    (if happy m then "" else "!") ++ show (cxt m) -- ++ (if Set.null (contradictors m) then "" else "(xor "++(show $ contradictors m)++")")
        -- ++ (if null (baseMites m) then "" else "(base " ++ show (baseMites m) ++ "/base)")
instance Ord Mite where compare m1 m2 = compare (xorKey m1) (xorKey m2)
instance Eq Mite where m1 == m2 = (xorKey m1) == (xorKey m2)

mite cxt = _initMite cxt Set.empty []

semS var prop value = mite $ Sem var (StrValue prop value)
semV var prop value = mite $ Sem var (VarValue prop value)
semT var _type = semS var Type _type

xorNonEmpty groups = if length nonEmpty <= 1 then concat nonEmpty else xor nonEmpty where nonEmpty = filter (not . null) groups

xor :: [[Mite]] -> [Mite]
xor _miteGroups = let
      sets = Set.fromList $ map Set.fromList _miteGroups
      nodups = if Set.size sets /= length _miteGroups then error $ "duplicate groups in xor " ++ show _miteGroups else _miteGroups
      miteGroups = diversify nodups
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
    if any null _miteGroups then error $ "Empty mite group: " ++ show miteGroups
    else if length _miteGroups <= 1 then concat _miteGroups
    else assert (LS.removeDups newMites == newMites) $ {-traceShow ("xor", miteGroups) $ traceShow ("->", newMites) $ -}newMites

contradict mite1 mite2 = any (\m -> Set.member (xorKey m) (flattenContradictors mite1)) (flattenBaseMites mite2) || differentGenerations where
  differentGenerations = flip any (Map.assocs $ generations mite2) $ \(ancestor, gen2) -> case Map.lookup ancestor (generations mite1) of
    Nothing -> False
    Just gen1 -> gen1 /= gen2

diversify miteGroups = result where
  mite2Count = Map.fromListWith (+) [(mite, 1) | mite <- concat miteGroups]
  isUnique mite = 1 == Map.findWithDefault 0 mite mite2Count
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

    pairs [] = []
    pairs (x:xs) = [(x, y)| y <- xs] ++ pairs xs

    issues = concat [if contradict m1 m2 then [(m1, m2)] else [] | (m1, m2) <- pairs newBase]
    prettyIssues = intercalate "\n" $ map (\(m1, m2) -> show m1 ++ "\n  contradicts\n  " ++ show m2) issues
    in
    if null issues
    then _initMite (cxt m) (Set.map addBaseToContra $ contradictors m) newBase
    else error $ "contradictory base for mite " ++ show m ++ ":\n" ++ prettyIssues ++ "\nallBase=" ++ show newBase
  in map handleMite mites

optional mites = xor [mites, [mite $ EmptyCxt $ cxt $ head mites]]

isCoverable mite = if not (happy mite) then True else case cxt mite of
  NomHead {} -> True
  GenHead {} -> True
  Clause {} -> True
  QuestionVariants {} -> True
  Unclosed {} -> True
  _ -> False

isHandicap (cxt -> Handicap _) = True
isHandicap _ = False

isInteractive mite = case cxt mite of
  Sem {} -> False
  Unify {} -> False
  EmptyCxt {} -> False
  Diversifier {} -> False
  _ -> True