module Constructor.Sense 
  (Sense(..), Frame(..), Fact(..), fValue, sValue, 
  usages, usage, 
  getType, hasType, hasAnyType, resolve, 
  allFrames, allFrameFacts,
  flatten, isNumber,
  makeSense)
  where

import Constructor.Constructions (Construction(Sem, Unify), Mite(..))
import Constructor.Variable
import Constructor.Tree
import Data.List (intercalate, findIndex, find)
import Data.Maybe
import Debug.Trace
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Constructor.LinkedSet as LS

calcFacts allMites baseVars =
  let mapper mite =
        case cxt mite of
          Sem var attr value ->
            let normalizedValue = case value of
                  StrValue _ -> value
                  VarValue var -> VarValue $ Map.findWithDefault var var baseVars
            in [Fact (Map.findWithDefault var var baseVars) attr normalizedValue]
          _ -> []
  in
  LS.removeDups $ concat $ map mapper allMites

calcBaseVars:: [Mite] -> Map.Map Variable Variable
calcBaseVars mites =
  let inner = \ groups mite ->
        case cxt mite of
          Unify var1 var2 ->
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

data Sense = Sense { facts:: [Fact] } deriving (Eq, Ord)
instance Show Sense where show (Sense facts) = Data.List.intercalate "\n" (map show facts)

data Frame = Frame { var:: Variable, sense:: Sense } deriving (Eq, Ord)
instance Show Frame where show frame = "{" ++ (Data.List.intercalate "," (map show $ allFrameFacts frame)) ++ "}"

makeSense trees =
  let allMites = concat (map allActiveMites $ reverse trees)
      baseVars = calcBaseVars allMites
  in Sense (calcFacts allMites baseVars)

extractValueString (StrValue s) = Just s
extractValueString _ = Nothing

extractValueVar (VarValue v) = Just v
extractValueVar _ = Nothing

allFrames sense = [Frame var sense | var <- LS.removeDups allVars ] where
  allVars = [variable fact | fact <- facts sense ] ++ valueVars
  valueVars = catMaybes [extractValueVar $ value fact | fact <- facts sense ]

allFrameFacts frame = [fact | fact <- facts (sense frame), var frame == variable fact]
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
        if hasAnyType ["CASE", "HAMMER", "3", "4", "7", "8", "TREES", "CHILD", "BENCH"] frame then Just "false"
        else if hasType "CASHIER" frame then
          case find (\shop -> earlier shop "type" frame "type") $ findFrames "SHOP" $ sense frame of
           Just shop -> sValue "given" shop
           _ -> Just "true"
        else if hasType "SHOP" frame then
          if any (\cashier -> earlier cashier "type" frame "type") $ findFrames "CASHIER" $ sense frame then Just "true"
          else Just "false"
        else Just "true"
      "type" ->
        case (sValue "rusNumber" frame, sValue "rusPerson" frame) of
          (Just "Pl", Just "3") -> Just "THEY"
          _ -> usage "arg1" frame >>= commandingSubject >>= getType 
      _ -> Nothing

fDeclaredValue attr frame = singleValue attr frame >>= extractValueVar >>= \v -> Just $ Frame v (sense frame)
fValue attr frame =
  let declared = fDeclaredValue attr frame in
  if isJust declared then declared
  else
    case attr of
      "arg1" ->
        if hasType "NEIGHBORS" frame then usage "goal" frame >>= fValue "arg1"
        else if hasType "MOUTH" frame then usage "source" frame >>= fValue "arg1"
        else Nothing
      _ -> Nothing

commandingSubject frame = usage "content" frame >>= usage "theme" >>= fValue "arg1"

hasType t frame = getType frame == Just t
hasAnyType types frame = fromMaybe False $ getType frame >>= \t -> Just $ elem t types
getType frame = sValue "type" frame

usages attr frame = [f | f <- allFrames (sense frame), fDeclaredValue attr f == Just frame]
usage attr frame = singleListElement $ usages attr frame

flatten Nothing = []
flatten (Just frame) = if hasType "seq" frame then flatten (fValue "member1" frame) ++ maybeToList (fValue "member2" frame) else [frame]

resolve frame = case (getType frame, fValue "target" frame) of
  (Just "SELF", Just target) -> target
  _ -> frame

isNumber frame = any (\f -> sValue "number" f == Just "true") $ flatten frame