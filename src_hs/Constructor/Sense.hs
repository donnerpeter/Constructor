module Constructor.Sense
  (Sense(..), Frame(..), Fact(..),
  allFrames,
  fValue, sValue,
  fDeclaredValue, sDeclaredValue,
  usages, usage, allUsages,
  getType, getDeclaredType, hasType, hasAnyType, resolve,
  earlier, typeEarlier,
  allFrameFacts,
  flatten, isNumber, unSeq, unSeq1, unSeq2, seqSiblings, prevSiblings, nextSiblings,
  isHuman, isAnimate, isInanimate,
  isCP, isFactCP, isQuestionCP,
  makeSense, composeSense)
  where

import Constructor.Variable
import qualified Constructor.SemanticProperties as P
import Constructor.Util
import Control.Monad
import Data.List (intercalate, findIndex, find, intersect, sort)
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Constructor.LinkedSet as LS

type EqClass = [Variable]
data EqClasses = EqClasses { baseVars:: Map.Map Variable Variable, eqClasses :: Map.Map Variable EqClass }

toBase (EqClasses baseVars _) var = Map.findWithDefault undefined var baseVars

addEqClass :: EqClasses -> EqClass -> (EqClasses, ClassUpdate)
addEqClass ec@(EqClasses baseVars eqClasses) vars = (EqClasses newBaseVars newClasses, ClassUpdate addedClasses removedClasses) where
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
  children :: Maybe (FactMap, FactMap)
}

composeFactMaps fm1 fm2 varClasses update = FactMap knownVars valueCache usageCache (Just (fm1, fm2)) where
  knownVars = Set.union (knownVariables fm1) (knownVariables fm2)
  updateCache getter = let
    withoutRemoved = Set.foldl (\m c -> Map.delete c m) (getter fm1) (removedClasses update)
    withAdded = Set.foldl (\m c -> Map.insert c (combineFacts getter fm1 fm2 c) m) withoutRemoved (addedClasses update)
    in withAdded
  valueCache = updateCache var2Values
  usageCache = updateCache var2Usages

combineFacts getter fm1 fm2 vars = if LS.isEmpty facts1 then facts2 else LS.union facts1 facts2 where
  facts1 = getFactsFromMap getter fm1 $ filter (flip Set.member (knownVariables fm1)) vars
  facts2 = getFactsFromMap getter fm2 $ filter (flip Set.member (knownVariables fm2)) vars

getFactsFromMap getter factMap vars = if null vars then LS.empty else Map.findWithDefault calcValue vars $ getter factMap where
  calcValue = case children factMap of
    Nothing -> foldl LS.union LS.empty [facts | (vs, facts) <- Map.toList $ getter factMap, not (null $ intersect vs vars)]
    Just (left, right) -> combineFacts getter left right vars

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
      children = Nothing
     }

cacheKey varClasses v = Map.findWithDefault [v] v $ eqClasses varClasses

makeSenseInternal bareFacts allFrameVars varClasses factMap = Sense allFrameVars varClasses factMap factCache usageCache bareFacts where
  factCache  = Map.fromList [(var, [Fact var (normalizeValue varClasses value) | value <- LS.elements $ getFactsFromMap var2Values factMap $ cacheKey varClasses var]) | var <- allFrameVars]
  usageCache = Map.fromList [(var, map (normalizeFact varClasses) $ LS.elements $ getFactsFromMap var2Usages factMap $ cacheKey varClasses var) | var <- allFrameVars]

normalizeFact varClasses (Fact var1 value) = Fact (toBase varClasses var1) (normalizeValue varClasses value)

normalizeValue varClasses value = case value of
  StrValue _ _ -> value
  VarValue prop var2 -> VarValue prop $ toBase varClasses var2

composeSense s1 s2 = makeSenseInternal _bareFacts _allFrameVars mergedClasses _factMap where
  _bareFacts = bareFacts s1 ++ bareFacts s2
  folder (ec1, u1) vars = let (ec2, u2) = addEqClass ec1 vars in (ec2, composeUpdates u1 u2)
  classesToAdd = Map.elems $ eqClasses $ varClasses s2
  (mergedClasses, compositeUpdate) = foldl folder (varClasses s1, ClassUpdate Set.empty Set.empty) classesToAdd
  _allFrameVars = LS.removeDups $ map (toBase mergedClasses) $ allFrameVars s1 ++ allFrameVars s2
  _factMap = composeFactMaps (factMap s1) (factMap s2) mergedClasses compositeUpdate

toFrames sense vars = map (flip Frame sense) vars
allFrames sense = toFrames sense $ allFrameVars sense
allFrameFacts frame = Map.findWithDefault [] (var frame) $ factCache (sense frame)
singleListElement list = case list of
  [single] -> Just single
  _ -> Nothing

findFrames typ sense = [f | f <- allFrames sense, hasType typ f]

typeEarlier f1 f2 = earlier f1 P.Type f2 P.Type

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

sValue attr frame =
  let declared = sDeclaredValue attr frame in
  if isJust declared then declared
  else
    case attr of
      P.Given ->
        if Just True == fmap (hasAnyType ["SOME", "ONE"]) (fValue P.Determiner frame) then Just "false"
        else if hasAnyType ["CASE", "HAMMER", "TREES", "BENCH", "FINGER", "WATERMELON", "JAW"] frame then Just "false"
        else if Just True == fmap isNumberString (getType frame) then Just "false"
        else if hasType "CHILD" frame then
          if Just "SOME" == (fValue P.Determiner frame >>= getType) then Just "false" else Just "true"
        else if hasType "CASHIER" frame then
          case find (\cashier -> typeEarlier cashier frame) $ findFrames "CASHIER" $ sense frame of
             Just prev -> Just "true"
             _ -> case find (\shop -> typeEarlier shop frame) $ findFrames "SHOP" $ sense frame of
               Just shop -> sValue P.Given shop
               _ -> Just "true"
        else if hasType "SHOP" frame then
          if any (\cashier -> typeEarlier cashier frame) $ findFrames "CASHIER" $ sense frame then Just "true"
          else if isJust $ msum [usage P.Arg1 frame, usage P.Source frame] then Just "true"
          else Just "false"
        else if Just True == fmap isCP (usage P.Content frame) then Just "false"
        else Just "true"
      P.Type -> case usage P.Arg1 frame >>= commandingSubject >>= getType of
        Just commandingType -> Just commandingType
        Nothing ->
          case (sDeclaredValue P.RusNumber frame, sDeclaredValue P.RusPerson frame, sDeclaredValue P.RusGender frame) of
            (Just "Pl", Just "3", _) -> Just "THEY"
            (Just "Sg", Just "3", Just "Fem") -> Just "SHE"
            (Just "Sg", Just "3", _) -> Just "HE"
            _ -> Nothing
      P.RusNumber -> case sDeclaredValue P.Type frame of
        Just "WE" -> Just "Pl"
        Just "THEY" -> Just "Pl"
        Just "ME" -> Just "Sg"
        _ -> Nothing
      _ -> Nothing

fDeclaredValue attr frame = singleListElement $ allFrameFacts frame >>= \case
  Fact {value=VarValue a v} | a == attr -> [Frame v (sense frame)]
  _ -> []

fValue attr frame =
  let declared = fDeclaredValue attr frame in
  if isJust declared then declared
  else
    case attr of
      P.Arg1 ->
        if hasAnyType ["MOUTH", "NOSE", "JAW", "JAWS", "FINGER", "NEIGHBORS", "CURIOSITY"] frame then let
          verbs = catMaybes [usage P.Source $ unVariants $ unSeq frame,
                             usage P.Arg2 $ unVariants $ unSeq frame,
                             usage P.Instrument $ unVariants $ unSeq frame,
                             usage P.Reason $ unVariants $ unSeq frame,
                             usage P.Goal_to $ unVariants $ unSeq frame]
          foregrounds = catMaybes $ map (usage P.PerfectBackground) verbs
          unVariants frame = unSeq $ fromMaybe frame (usage P.Variants frame)
          in fmap resolve $ msum $ map (fValue P.Receiver) (verbs ++ foregrounds) ++ map (fValue P.Arg1) (verbs ++ foregrounds)
        else Nothing
      _ -> Nothing

commandingSubject frame = msum [usage P.Content frame >>= usage P.Theme, usage P.Content frame >>= usage P.Arg2] >>= fValue P.Arg1

hasType t frame = getType frame == Just t
hasAnyType types frame = fromMaybe False $ getType frame >>= \t -> Just $ elem t types
getType frame = sValue P.Type frame
getDeclaredType frame = sDeclaredValue P.Type frame

usageFacts frame = Map.findWithDefault [] (var frame) $ usageCache (sense frame)
allUsages attrs frame = toFrames (sense frame) $ [v | Fact {variable=v, value=VarValue s _} <- usageFacts frame, s `elem` attrs]
usages attr frame = toFrames (sense frame)  $ [v | Fact {variable=v, value=VarValue s _} <- usageFacts frame, s == attr]
usage attr frame = singleListElement $ usages attr frame

flatten Nothing = []
flatten (Just frame) = if Just "seq" == getDeclaredType frame then flatten (fValue P.Member1 frame) ++ maybeToList (fValue P.Member2 frame) else [frame]

seqSiblings frame = flatten $ Just $ unSeq frame
prevSiblings frame = takeWhile (/= frame) $ seqSiblings frame
nextSiblings frame = tail $ dropWhile (/= frame) $ seqSiblings frame

resolve frame = case getType frame of
  Just "SELF" | Just target <- fValue P.Target frame -> target
  Just "wh" | Just relativized <- usage P.Questioned frame >>= usage P.Relative -> relativized
  _ -> frame

isNumber frame = any (\f -> sValue P.Number f == Just "true") $ flatten frame

isHuman frame = hasAnyType ["NEIGHBOR", "NEIGHBORS", "CASHIER", "NAMED_PERSON", "EVERYBODY",
  "ME", "HE", "SHE", "THEY" -- todo pronouns are not necessarily animate
  ] frame
isAnimate frame = isHuman frame || Just "true" == sValue P.Animate frame

isInanimate frame = Just "wh" == getDeclaredType frame && Just "true" /= sValue P.Animate frame
                 || Just True == fmap isNumberString (getType frame)

unSeq frame = case msum [usage P.Member1 frame, usage P.Member2 frame] of
  Just s -> unSeq s
  _ -> frame

unSeq1 frame = case usage P.Member1 frame of
  Just s -> unSeq1 s
  _ -> frame

unSeq2 frame = case usage P.Member2 frame of
  Just s -> unSeq2 s
  _ -> frame

isCP frame = Just "situation" == getDeclaredType frame

isFactCP frame = isCP frame && not (isQuestionCP frame)
isQuestionCP frame = isCP frame && isJust (fValue P.Questioned frame)
