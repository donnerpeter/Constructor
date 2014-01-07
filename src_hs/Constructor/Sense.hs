module Constructor.Sense where

import Constructor.Constructions
import Constructor.Tree
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import qualified Data.Set as Set

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

data Fact = Fact { variable:: Variable, attrName:: String, value:: SemValue } deriving (Eq)
instance Show Fact where show (Fact var attr value) = (show var)++"."++attr++"="++(show value)

data Sense = Sense { facts:: [Fact] } deriving (Eq)
instance Show Sense where show (Sense facts) = Data.List.intercalate "\n" (map show facts)

data Frame = Frame { var:: Variable, sense:: Sense } deriving (Eq, Show)

makeSense trees =
  let allMites = concat (map allTreeMites $ reverse trees)
      baseVars = calcBaseVars allMites
  in Sense (calcFacts allMites baseVars)

extractValueString (StrValue s) = Just s
extractValueString _ = Nothing

extractValueVar (VarValue v) = Just v
extractValueVar _ = Nothing

allFrames sense = [Frame var sense | var <- Set.elems $ Set.fromList allVars ] where
  allVars = [variable fact | fact <- facts sense ] ++ valueVars
  valueVars = catMaybes [extractValueVar $ value fact | fact <- facts sense ]

allFrameFacts frame = [fact | fact <- facts (sense frame), var frame == variable fact]
allValues frame attr = [value fact | fact <- allFrameFacts frame, attrName fact == attr]
singleValue frame attr = case allValues frame attr of
  [single] -> Just single
  _ -> Nothing
sValue frame attr = singleValue frame attr >>= extractValueString
fValue frame attr = singleValue frame attr >>= extractValueVar >>= \v -> Just $ Frame v (sense frame)
hasType frame t = getType frame == Just t
getType frame = sValue frame "type"