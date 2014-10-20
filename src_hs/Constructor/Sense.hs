{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Constructor.Sense
  (Sense(..), Frame(..), Fact(..),
  fValue, sValue,
  fDeclaredValue, sDeclaredValue,
  usages, usage, allUsages,
  getType, getDeclaredType, hasType, hasAnyType, resolve,
  earlier, typeEarlier,
  allFrameFacts,
  flatten, isNumber, unSeq, seqSiblings, prevSiblings, nextSiblings,
  isHuman, isAnimate, isInanimate,
  isCP, isFactCP, isQuestionCP,
  makeSense)
  where

import Constructor.Constructions (Construction(Sem, Unify))
import Constructor.Mite (Mite(..))
import Constructor.Variable
import qualified Constructor.SemanticProperties as P
import Constructor.Util
import Control.Monad
import Data.List (intercalate, findIndex, find)
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Constructor.LinkedSet as LS

calcFacts allMites baseVars =
  let mapper = \case
          (cxt -> Sem var value) ->
            let normalizedValue = case value of
                  StrValue _ _ -> value
                  VarValue prop var -> VarValue prop $ Map.findWithDefault var var baseVars
            in [Fact (Map.findWithDefault var var baseVars) normalizedValue]
          _ -> []
  in
  LS.removeDups $ concat $ map mapper allMites

calcBaseVars:: [Mite] -> Map.Map Variable Variable
calcBaseVars mites =
  let inner = \ groups -> \case
          (cxt -> Unify var1 var2) ->
            let ensured = ensureGroup var1 $ ensureGroup var2 groups
                mergedGroup = Set.union (ensured Map.! var1) (ensured Map.! var2)
                groupsMap = foldl (\ groups var -> Map.insert var mergedGroup groups) groups (Set.elems mergedGroup)
            in groupsMap
          _ -> groups
      ensureGroup = \ var groups ->
        if Map.member var groups then groups
        else Map.insert var (Set.singleton var) groups
      groups = foldl inner Map.empty mites
  in Map.map (head . Set.elems) groups

data Fact = Fact { variable:: Variable, value:: SemValue } deriving (Eq, Ord)
instance Show Fact where show (Fact var value) = (show var)++"."++(show value)

data Sense = Sense { facts:: [Fact], allFrames:: [Frame], frame2Facts:: Map.Map Frame [Fact], frame2Usages:: Map.Map Frame [(Frame, P.VarProperty)] }
instance Show Sense where show sense = Data.List.intercalate "\n" (map show $ facts sense)
instance Eq Sense where s1 == s2 = facts s1 == facts s2
instance Ord Sense where compare s1 s2 = compare (facts s1) (facts s2)

data Frame = Frame { var:: Variable, sense:: Sense } deriving (Eq)
instance Show Frame where show frame = "{" ++ (Data.List.intercalate "," (map show $ allFrameFacts frame)) ++ "}"
instance Ord Frame where compare s1 s2 = compare (var s1) (var s2)

makeSense allMites = sense where
  sense = Sense facts allFrames frame2Facts frame2Usages
  facts = calcFacts allMites $ calcBaseVars allMites

  allFrames = [Frame var sense | var <- LS.removeDups allVars ]
  allVars = [variable fact | fact <- facts ] ++ valueVars
  valueVars = facts >>= \case
    Fact {value=VarValue _ v} -> [v]
    _ -> []

  frame2Facts = Map.fromListWith (flip (++)) [(Frame (variable fact) sense, [fact]) | fact <- facts]

  frame2Usages = Map.fromListWith (flip (++)) $ facts >>= \case
    Fact {variable=var, value=VarValue prop v} -> [(Frame v sense, [(Frame var sense, prop)])]
    _ -> []

allFrameFacts frame = Map.findWithDefault [] frame $ frame2Facts (sense frame)
singleListElement list = case list of
  [single] -> Just single
  _ -> Nothing

findFrames typ sense = [f | f <- allFrames sense, hasType typ f]

typeEarlier f1 f2 = earlier f1 P.Type f2 P.Type

earlier f1 attr1 f2 attr2 =
  let allFacts = facts $ sense f1
      mi1 = findIndex (isStrAssignment (var f1) attr1) allFacts
      mi2 = findIndex (isStrAssignment (var f2) attr2) allFacts
      isStrAssignment var attr = \case
        Fact {variable=v, value=StrValue a _} | a == attr && v == var -> True
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
        if hasAnyType ["CASE", "HAMMER", "TREES", "BENCH", "FINGER", "WATERMELON", "JAW"] frame then Just "false"
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
        if hasAnyType ["MOUTH", "NOSE", "JAW", "JAWS", "FINGER", "NEIGHBORS"] frame then let
          verbs = catMaybes [usage P.Source $ unVariants $ unSeq frame,
                             usage P.Arg2 $ unVariants $ unSeq frame,
                             usage P.Instrument $ unVariants $ unSeq frame,
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

allUsages attrs frame = LS.removeDups $ [f | (f, s) <- Map.findWithDefault [] frame $ frame2Usages (sense frame), s `elem` attrs]
usages attr frame = LS.removeDups $ [f | (f, s) <- Map.findWithDefault [] frame $ frame2Usages (sense frame), s == attr]
usage attr frame = singleListElement $ usages attr frame

flatten Nothing = []
flatten (Just frame) = if Just "seq" == getDeclaredType frame then flatten (fValue P.Member1 frame) ++ maybeToList (fValue P.Member2 frame) else [frame]

seqSiblings frame = flatten $ Just $ unSeq frame
prevSiblings frame = takeWhile (/= frame) $ seqSiblings frame
nextSiblings frame = tail $ dropWhile (/= frame) $ seqSiblings frame

resolve frame = case (getType frame, fValue P.Target frame) of
  (Just "SELF", Just target) -> target
  _ -> frame

isNumber frame = any (\f -> sValue P.Number f == Just "true") $ flatten frame

isHuman frame = hasAnyType ["NEIGHBOR", "NEIGHBORS", "CASHIER", "NAMED_PERSON", "EVERYBODY",
  "HE", "SHE", "THEY" -- todo pronouns are not necessarily animate
  ] frame
isAnimate frame = isHuman frame || Just "true" == sValue P.Animate frame

isInanimate frame = Just "wh" == getDeclaredType frame && Just "true" /= sValue P.Animate frame
                 || Just True == fmap isNumberString (getType frame)

unSeq frame = case msum [usage P.Member1 frame, usage P.Member2 frame] of
  Just s -> unSeq s
  _ -> frame

isCP frame = Just "situation" == getDeclaredType frame

isFactCP frame = isCP frame && not (isQuestionCP frame)
isQuestionCP frame = isCP frame && isJust (fValue P.Questioned frame)
