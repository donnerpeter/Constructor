module Constructor.Sense
  (Sense(..), Frame(..), Fact(..),
  allFrames, allFrameFacts, toFrame,
  fDeclaredValue, sDeclaredValue,
  earlier,
  usages, usage, allUsages, usageFacts,
  isFrameReachable, reachableFrames, framesTo,
  makeSense, composeSense)
  where

import Constructor.Variable
import Data.List (intercalate, findIndex, intersect, sort)
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Constructor.LinkedSet as LS

type EqClass = [Variable]
data EqClasses = EqClasses { baseVars:: Map.Map Variable Variable, eqClasses :: Map.Map Variable EqClass }

toBase (EqClasses baseVars _) var = baseVars Map.! var

addEqClass :: EqClasses -> EqClass -> (EqClasses, ClassUpdate)
addEqClass (EqClasses baseVars eqClasses) vars = (EqClasses newBaseVars newClasses, ClassUpdate addedClasses removedClasses) where
  basesToUnify = catMaybes $ map (\v -> Map.lookup v baseVars) vars
  mergedClass = sort $ LS.removeDups $ concat (map (\v -> eqClasses Map.! v) basesToUnify) ++ vars
  singleBase = head mergedClass
  newClasses = Map.insert singleBase mergedClass $ foldl (\m v -> Map.delete v m) eqClasses basesToUnify
  newBaseVars = foldl (\m v -> Map.insert v singleBase m) baseVars mergedClass
  addedClasses = Set.fromList [mergedClass]
  removedClasses = Set.fromList $ map (\v -> eqClasses Map.! v) basesToUnify

data ClassUpdate = ClassUpdate { removedClasses:: Set.Set EqClass, addedClasses:: Set.Set EqClass }

composeUpdates u1 u2 = ClassUpdate added removed where
  added = Set.union (Set.difference (addedClasses u1) (removedClasses u2)) (addedClasses u2)
  removed = Set.union (removedClasses u1)  (removedClasses u2)

data Fact = Fact { variable:: Variable, value:: SemValue } deriving (Eq, Ord)
instance Show Fact where show (Fact var value) = (show var)++"."++(show value)

data FactMap = FactMap {
  knownVariables :: Set.Set Variable,
  var2Values :: Map.Map EqClass (LS.LinkedSet SemValue),
  var2Usages :: Map.Map EqClass (LS.LinkedSet Fact),
  children :: [FactMap]
}

type MapGetter a = FactMap -> Map.Map EqClass (LS.LinkedSet a)

composeFactMaps factMaps baseFM update = FactMap knownVars valueCache usageCache factMaps where
  knownVars = foldl Set.union Set.empty $ map knownVariables factMaps
  updateCache getter = let
    withoutRemoved = Set.foldl (\m c -> Map.delete c m) (getter baseFM) (removedClasses update)
    withAdded = Set.foldl (\m c -> Map.insert c (combineFacts getter factMaps c) m) withoutRemoved (addedClasses update)
    in withAdded
  valueCache = updateCache var2Values
  usageCache = updateCache var2Usages

combineFacts :: (Ord a) => MapGetter a -> [FactMap] -> [Variable] -> LS.LinkedSet a
combineFacts getter factMaps vars = if length facts == 1 then head facts else foldl LS.union LS.empty facts where
  facts = filter (not . LS.isEmpty) $ map (\fm -> getFactsFromMap getter fm $ filter (flip Set.member (knownVariables fm)) vars) factMaps

getFactsFromMap :: (Ord a) => MapGetter a -> FactMap -> [Variable] -> LS.LinkedSet a
getFactsFromMap getter factMap vars = if null vars then LS.empty else Map.findWithDefault calcValue vars $ getter factMap where
  calcValue =
    if null $ children factMap
    then foldl LS.union LS.empty [facts | (vs, facts) <- Map.toList $ getter factMap, not (null $ intersect vs vars)]
    else combineFacts getter (children factMap) vars

data Sense = Sense {
  allFrameVars:: [Variable],
  varClasses:: EqClasses,
  factMap:: FactMap,
  factCache:: Map.Map Variable [Fact],
  usageCache:: Map.Map Variable [Fact],
  bareFacts:: [Fact]
  }
instance Show Sense where
  show sense =
    Data.List.intercalate "\n" (map show $ LS.removeDups $ map (normalizeFact $ varClasses sense) $ bareFacts sense)
instance Eq Sense where s1 == s2 = bareFacts s1 == bareFacts s2

data Frame = Frame { var:: Variable, sense:: Sense } deriving (Eq)
instance Show Frame where show frame = "{" ++ (Data.List.intercalate "," (map show $ allFrameFacts frame)) ++ "}"
instance Ord Frame where compare s1 s2 = compare (var s1) (var s2)

makeSense bareFacts unifications = makeSenseInternal bareFacts allBaseVars varClasses factMap where
  factVars = LS.removeDups $ map variable bareFacts ++ [v | Fact {value=VarValue _ v} <- bareFacts]
  singleVarClasses = foldl (\ec var -> fst $ addEqClass ec [var]) (EqClasses Map.empty Map.empty) factVars
  varClasses = foldl (\ec (var1, var2) -> fst $ addEqClass ec [var1, var2]) singleVarClasses unifications
  allBaseVars = LS.removeDups $ factVars ++ Map.keys (baseVars varClasses)
  cacheMap pairs = let base2Facts = Map.fromListWith (flip (++)) [(toBase varClasses v, facts) | (v, facts) <- pairs]
    in Map.fromList [(cacheKey varClasses v, LS.fromList $ Map.findWithDefault [] v base2Facts) | v <- allBaseVars]
  factMap = FactMap {
      knownVariables = Map.keysSet (baseVars varClasses),
      var2Values = cacheMap [(variable fact, [value fact]) | fact <- bareFacts],
      var2Usages = cacheMap [(v, [fact]) | fact@(Fact {value=VarValue _ v}) <- bareFacts],
      children = []
     }

cacheKey varClasses v = Map.findWithDefault [v] v $ eqClasses varClasses

makeSenseInternal bareFacts allFrameVars varClasses factMap = Sense allFrameVars varClasses factMap factCache usageCache bareFacts where
  factCache  = Map.fromList [(var, [Fact var (normalizeValue varClasses value) | value <- LS.elements $ getFactsFromMap var2Values factMap $ cacheKey varClasses var]) | var <- allFrameVars]
  usageCache = Map.fromList [(var, map (normalizeFact varClasses) $ LS.elements $ getFactsFromMap var2Usages factMap $ cacheKey varClasses var) | var <- allFrameVars]

normalizeFact varClasses (Fact var1 value) = Fact (toBase varClasses var1) (normalizeValue varClasses value)

toFrame sense var = Frame (toBase (varClasses sense) var) sense

normalizeValue varClasses value = case value of
  StrValue _ _ -> value
  VarValue prop var2 -> VarValue prop $ toBase varClasses var2

composeSense senses = makeSenseInternal _bareFacts _allFrameVars mergedClasses _factMap where
  _bareFacts = senses >>= bareFacts
  baseIndex = snd $ maximum $ zip (map (Map.size . eqClasses . varClasses) senses) [0..]
  baseSense = senses !! baseIndex
  addedSenses = take baseIndex senses ++ drop (baseIndex + 1) senses
  folder (ec1, u1) vars = let (ec2, u2) = addEqClass ec1 vars in (ec2, composeUpdates u1 u2)
  classesToAdd = addedSenses >>= Map.elems . eqClasses . varClasses
  (mergedClasses, compositeUpdate) = foldl folder (varClasses baseSense, ClassUpdate Set.empty Set.empty) classesToAdd
  _allFrameVars = LS.removeDups $ map (toBase mergedClasses) $ (senses >>= allFrameVars)
  _factMap = composeFactMaps (map factMap senses) (factMap baseSense) compositeUpdate

toFrames sense vars = map (flip Frame sense) vars
allFrames sense = toFrames sense $ allFrameVars sense
allFrameFacts frame = Map.findWithDefault [] (var frame) $ factCache (sense frame)
singleListElement list = case list of
  [single] -> Just single
  _ -> Nothing

earlier f1 attr1 f2 attr2 =
  let allFacts = bareFacts $ sense f1
      eqClasses = varClasses $ sense f1
      mi1 = findIndex (isStrAssignment (var f1) attr1) allFacts
      mi2 = findIndex (isStrAssignment (var f2) attr2) allFacts
      isStrAssignment var attr = \case
        Fact {variable=v, value=StrValue a _} | a == attr && toBase eqClasses v == var -> True
        _ -> False
  in case (mi1, mi2) of
    (Just i1, Just i2) | i1 < i2 -> True
    _ -> False

sDeclaredValue attr frame = singleListElement $ allFrameFacts frame >>= \case
  Fact {value=StrValue a s} | a == attr -> [s]
  _ -> []

fDeclaredValue attr frame = singleListElement $ allFrameFacts frame >>= \case
  Fact {value=VarValue a v} | a == attr -> [Frame v (sense frame)]
  _ -> []

usageFacts frame = Map.findWithDefault [] (var frame) $ usageCache (sense frame)
allUsages attrs frame = toFrames (sense frame) $ [v | Fact {variable=v, value=VarValue s _} <- usageFacts frame, s `elem` attrs]
usages attr frame = toFrames (sense frame)  $ [v | Fact {variable=v, value=VarValue s _} <- usageFacts frame, s == attr]
usage attr frame = singleListElement $ usages attr frame

reachableFrames :: Frame -> Set.Set Frame
reachableFrames origin = visitFrame Set.empty origin where
  visitFrame visited frame = let
    neighbours = Set.fromList [toFrame (sense frame) v | (Fact {value=(VarValue _ v)}) <- allFrameFacts frame ]
    goFurther = Set.foldl visitFrame (Set.insert frame visited) neighbours
    in if Set.member frame visited then visited else goFurther

isFrameReachable src dest = Set.member dest (reachableFrames src)

framesTo :: Frame -> Frame -> Int -> Maybe (Set.Set Frame)
framesTo src dst maxPathLength = inner Set.empty src where
  sens = sense src
  inner path current = let
    allNeighbors = [toFrame sens v | (Fact {value=(VarValue _ v)}) <- allFrameFacts current]
    newNeighbors = filter (not . flip Set.member path) allNeighbors
    pathWithCurrent = Set.insert current path
    in if current == dst then Just path
       else if Set.size path >= maxPathLength then Nothing
       else Just $ Set.unions $ mapMaybe (inner pathWithCurrent) newNeighbors