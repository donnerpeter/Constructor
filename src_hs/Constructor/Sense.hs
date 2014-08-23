{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Constructor.Sense
  (Sense(..), Frame(..), Fact(..),
  fValue, sValue,
  fDeclaredValue, sDeclaredValue,
  usages, usage, allUsages,
  getType, hasType, hasAnyType, resolve,
  earlier,
  allFrameFacts,
  flatten, isNumber, unSeq, seqSiblings, prevSiblings, nextSiblings,
  isHuman, isAnimate, isInanimate,
  makeSense)
  where

import Constructor.Constructions (Construction(Sem, Unify))
import Constructor.Mite (Mite(..))
import Constructor.Variable
import Constructor.Util
import Control.Monad
import Data.List (intercalate, findIndex, find)
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Constructor.LinkedSet as LS

calcFacts allMites baseVars =
  let mapper = \case
          (cxt -> Sem var attr value) ->
            let normalizedValue = case value of
                  StrValue _ -> value
                  VarValue var -> VarValue $ Map.findWithDefault var var baseVars
            in [Fact (Map.findWithDefault var var baseVars) attr normalizedValue]
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

data Fact = Fact { variable:: Variable, attrName:: String, value:: SemValue } deriving (Eq, Ord)
instance Show Fact where show (Fact var attr value) = (show var)++"."++attr++"="++(show value)

data Sense = Sense { facts:: [Fact], allFrames:: [Frame], frame2Facts:: Map.Map Frame [Fact], frame2Usages:: Map.Map Frame [(Frame, String)] }
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
  valueVars = catMaybes [extractValueVar $ value fact | fact <- facts ]

  frame2Facts = Map.fromListWith (flip (++)) [(Frame (variable fact) sense, [fact]) | fact <- facts]

  frame2Usages = Map.fromListWith (flip (++)) $ facts >>= \fact -> case extractValueVar (value fact) of
    Just v -> [(Frame v sense, [(Frame (variable fact) sense, attrName fact)])]
    Nothing -> []

extractValueString (StrValue s) = Just s
extractValueString _ = Nothing

extractValueVar (VarValue v) = Just v
extractValueVar _ = Nothing

allFrameFacts frame = Map.findWithDefault [] frame $ frame2Facts (sense frame)
allValues attr frame = [value fact | fact <- allFrameFacts frame, attrName fact == attr]
singleListElement list = case list of
  [single] -> Just single
  _ -> Nothing
singleValue attr frame = singleListElement $ allValues attr frame

findFrames typ sense = [f | f <- allFrames sense, hasType typ f]

earlier f1 attr1 f2 attr2 =
  let allFacts = facts $ sense f1
      mi1 = findIndex (\ fact -> variable fact == var f1 && attrName fact == attr1) allFacts
      mi2 = findIndex (\ fact -> variable fact == var f2 && attrName fact == attr2) allFacts
  in case (mi1, mi2) of
    (Just i1, Just i2) | i1 < i2 -> True
    _ -> False

sDeclaredValue attr frame = singleValue attr frame >>= extractValueString
sValue attr frame =
  let declared = sDeclaredValue attr frame in
  if isJust declared then declared
  else
    case attr of
      "given" ->
        if hasAnyType ["CASE", "HAMMER", "TREES", "BENCH", "FINGER", "WATERMELON", "JAW"] frame then Just "false"
        else if Just True == fmap isNumberString (getType frame) then Just "false"
        else if hasType "CHILD" frame then
          if Just "SOME" == (fValue "determiner" frame >>= getType) then Just "false" else Just "true"
        else if hasType "CASHIER" frame then
          case find (\shop -> earlier shop "type" frame "type") $ findFrames "SHOP" $ sense frame of
           Just shop -> sValue "given" shop
           _ -> Just "true"
        else if hasType "SHOP" frame then
          if any (\cashier -> earlier cashier "type" frame "type") $ findFrames "CASHIER" $ sense frame then Just "true"
          else if isJust $ msum [usage "arg1" frame, usage "source" frame] then Just "true"
          else Just "false"
        else Just "true"
      "type" -> case usage "arg1" frame >>= commandingSubject >>= getType of
        Just commandingType -> Just commandingType
        Nothing ->
          case (sDeclaredValue "rusNumber" frame, sDeclaredValue "rusPerson" frame, sDeclaredValue "rusGender" frame) of
            (Just "Pl", Just "3", _) -> Just "THEY"
            (Just "Sg", Just "3", Just "Fem") -> Just "SHE"
            (Just "Sg", Just "3", _) -> Just "HE"
            _ -> Nothing
      "rusNumber" -> case sDeclaredValue "type" frame of
        Just "WE" -> Just "Pl"
        Just "THEY" -> Just "Pl"
        Just "ME" -> Just "Sg"
        _ -> Nothing
      _ -> Nothing

fDeclaredValue attr frame = singleValue attr frame >>= extractValueVar >>= \v -> Just $ Frame v (sense frame)
fValue attr frame =
  let declared = fDeclaredValue attr frame in
  if isJust declared then declared
  else
    case attr of
      "arg1" ->
        if hasAnyType ["MOUTH", "NOSE", "JAW", "JAWS", "FINGER", "NEIGHBORS"] frame then let
          verbs = catMaybes [usage "source" $ unVariants $ unSeq frame, usage "arg2" $ unVariants $ unSeq frame, usage "goal_to" $ unVariants $ unSeq frame]
          foregrounds = catMaybes $ map (usage "perfectBackground") verbs
          unVariants frame = unSeq $ fromMaybe frame (usage "variants" frame)
          in fmap resolve $ msum $ map (fValue "receiver") (verbs ++ foregrounds) ++ map (fValue "arg1") (verbs ++ foregrounds)
        else Nothing
      _ -> Nothing

commandingSubject frame = msum [usage "content" frame >>= usage "theme", usage "content" frame >>= usage "arg2"] >>= fValue "arg1"

hasType t frame = getType frame == Just t
hasAnyType types frame = fromMaybe False $ getType frame >>= \t -> Just $ elem t types
getType frame = sValue "type" frame

allUsages attrs frame = LS.removeDups $ [f | (f, s) <- Map.findWithDefault [] frame $ frame2Usages (sense frame), s `elem` attrs]
usages attr frame = LS.removeDups $ [f | (f, s) <- Map.findWithDefault [] frame $ frame2Usages (sense frame), s == attr]
usage attr frame = singleListElement $ usages attr frame

flatten Nothing = []
flatten (Just frame) = if hasType "seq" frame then flatten (fValue "member1" frame) ++ maybeToList (fValue "member2" frame) else [frame]

seqSiblings frame = flatten $ Just $ unSeq frame
prevSiblings frame = takeWhile (/= frame) $ seqSiblings frame
nextSiblings frame = tail $ dropWhile (/= frame) $ seqSiblings frame

resolve frame = case (getType frame, fValue "target" frame) of
  (Just "SELF", Just target) -> target
  _ -> frame

isNumber frame = any (\f -> sValue "number" f == Just "true") $ flatten frame

isHuman frame = hasAnyType ["NEIGHBOR", "NEIGHBORS", "CASHIER", "NAMED_PERSON",
  "HE", "SHE", "THEY" -- todo pronouns are not necessarily animate
  ] frame
isAnimate frame = isHuman frame || Just "true" == sValue "animate" frame

isInanimate frame = hasType "wh" frame && Just "true" /= sValue "animate" frame || Just True == fmap isNumberString (getType frame)

unSeq frame = case msum [usage "member1" frame, usage "member2" frame] of
  Just s -> unSeq s
  _ -> frame