module Constructor.Sense 
  (Sense(..), Frame(..), fValue, sValue, 
  usages, usage, 
  getType, hasType, hasAnyType, 
  allFrames, makeSense)
  where

import Constructor.Constructions (SemValue(..), Construction(Sem, Unify), Mite(..), Variable(..))
import Constructor.Tree
import Data.List (intercalate)
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
  concat [mapper mite | mite <- allMites]

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

allFrames sense = [Frame var sense | var <- LS.elements $ LS.fromList allVars ] where
  allVars = [variable fact | fact <- facts sense ] ++ valueVars
  valueVars = catMaybes [extractValueVar $ value fact | fact <- facts sense ]

allFrameFacts frame = [fact | fact <- facts (sense frame), var frame == variable fact]
allValues attr frame = [value fact | fact <- allFrameFacts frame, attrName fact == attr]
singleListElement list = case list of
  [single] -> Just single
  _ -> Nothing
singleValue attr frame = singleListElement $ allValues attr frame

sDeclaredValue attr frame = singleValue attr frame >>= extractValueString
sValue attr frame =
  let declared = sDeclaredValue attr frame in
  if isJust declared then declared
  else
    case attr of
      "given" ->
        if hasType "ORDER" frame then Just "true"
        else Just "false"
      "type" ->
        usage "arg1" frame >>= commandingSubject >>= getType 
      _ -> Nothing

fDeclaredValue attr frame = singleValue attr frame >>= extractValueVar >>= \v -> Just $ Frame v (sense frame)
fValue attr frame =
  let declared = fDeclaredValue attr frame in
  if isJust declared then declared
  else
    case attr of
      "arg1" ->
        if hasType "NEIGHBORS" frame then usage "goal" frame >>= fValue "arg1"
        else Nothing
      _ -> Nothing

commandingSubject frame = usage "content" frame >>= usage "theme" >>= fValue "arg1"

hasType t frame = getType frame == Just t
hasAnyType types frame = fromMaybe False $ getType frame >>= \t -> Just $ elem t types
getType frame = sValue "type" frame

usages attr frame = [f | f <- allFrames (sense frame), fDeclaredValue attr f == Just frame]
usage attr frame = singleListElement $ usages attr frame
  