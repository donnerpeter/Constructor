module Constructor.ArgumentPlanning (Argument(..), argumentFrame, arguments, isCP, isFactCP, isQuestionCP, isVerbEllipsis, isEllipsisAnchor) where
import Constructor.Sense
import Data.List
import Data.Maybe
import Constructor.Variable

isCPOrSeq frame = any isCP $ flatten $ Just frame

isVerbEllipsis verb = Just "true" == (usage "content" verb >>= sValue "ellipsis")

isEllipsisAnchor arg fVerb = isJust arg && (arg == (cp >>= fValue "ellipsisAnchor1") || arg == (cp >>= fValue "ellipsisAnchor2")) where
  cp = usage "content" fVerb

data Argument = Adverb String | NPArg Frame | PPArg String Frame | PPAdjunct String Frame deriving (Eq,Show)

argumentFrame (NPArg f) = Just f
argumentFrame (PPArg _ f) = Just f
argumentFrame _ = Nothing

arguments fVerb = reorderArgs $ fromMaybe [] $ flip fmap (getType fVerb) $ \typ -> let
  sens = sense fVerb
  compareFacts f1@(Fact frame1 attr1 val1) f2@(Fact frame2 attr2 val2) =
    if f1 == f2 then EQ
    else case (val1, val2) of
      (VarValue v1, VarValue v2) |
        fVal1 <- Frame v1 sens,
        fVal2 <- Frame v2 sens,
        isJust (getType $ Frame v1 sens) && isJust (getType $ Frame v2 sens) ->
          if earlier fVal1 "type" fVal2 "type" then LT else GT
      _ -> EQ
  in Data.List.sortBy compareFacts (allFrameFacts fVerb) >>= \(Fact _ attr semValue) ->
  case semValue of
    VarValue v -> let value = Frame v sens in
     if isVerbEllipsis fVerb && not (isEllipsisAnchor (Just value) fVerb) then [] else
     case (typ, attr) of
      ("COME_SCALARLY", "order") -> case getType value of
        Just "EARLIER" -> case fValue "anchor" value of
          Just anchor -> [PPArg "before" anchor]
          _ -> [Adverb "first"]
        Just "NEXT" -> [Adverb "next"]
        Just "AFTER" -> case fValue "anchor" value of
          Just anchor -> [PPArg "after" anchor]
          _ -> [Adverb "after"]
        Just "BEFORE" -> case fValue "anchor" value of
          Just anchor -> [PPArg "before" anchor]
          _ -> [Adverb "before"]
        _ -> []
      ("HAPPEN", "experiencer") -> [PPArg "to" value]
      ("TAKE_OUT", "source") -> [PPArg "out of" value]
      ("RUN_OUT", "source") -> [PPArg "out of" value]
      ("FALL", "source") -> [PPArg "off" value]
      ("SAY", "addressee") -> [NPArg value]
      ("ASK", "topic") ->
        if all isQuestionCP $ flatten $ Just value then []
        else [PPArg (if hasType "PREDICAMENT" value then "on" else "about") value]
      ("LACK", "theme") -> [NPArg value]
      ("DISTRACT", "theme") -> [PPArg "from" value]
      ("THINK", "topic") -> [PPArg "on" value]
      ("SEEM", "experiencer") -> if isJust (usage "content" fVerb >>= usage "reason") then [] else [PPAdjunct "to" value]
      ("SEEM", "theme") ->
        if hasType "LACK" value then [PPArg "void of" (fromJust $ fValue "theme" value)]
        else if hasType "MEANINGLESS" value then [Adverb "meaningless"]
        else if hasType "CLEVER" value then [Adverb "clever"]
        else [Adverb (fromMaybe "??" $ getType value)]
      ("DISPERSE", "goal") -> if hasType "HOMES" value then [Adverb "home"] else [PPArg "to" value]
      ("GO", "goal_action") -> if hasType "WALK" value then [Adverb "for a walk"] else [PPArg "to" value]
      (_, "goal") -> if typ == "GO" && hasType "HOME" value then [Adverb "home"] else [PPArg "to" value]
      (_, "goal_to") -> [PPArg "to" value]
      (_, "goal_in") -> [PPArg "to" value]
      (_, "goal_on") -> [PPArg "to" value]
      (_, "source") -> [PPArg "from" value]
      (_, "instrument") -> [PPArg "with" value]
      (_, "mood") -> case getType value of
        Just "JOY" | isNothing (fValue "size" value)-> [Adverb "cheerfully"]
        Just _ -> [PPAdjunct "with" value]
        _ -> []
      (_, "location") -> if hasType "THERE" value then [] else [PPArg "on" value]
      (_, "arg2") -> if isCPOrSeq value then [] else [NPArg value]
      (_, "duration") -> if hasType "LONG" value then [Adverb "for a long time"] else []
      (_, "relTime") -> case fValue "anchor" value of
        Just anchor -> [PPAdjunct (if hasType "AFTER" value then "after" else "before") anchor]
        _ -> []
      _ -> []
    StrValue value -> case (attr, value) of
      ("anchor", "AGAIN") -> [Adverb "again"]
      ("anchor", "ALREADY") -> [Adverb "already"]
      _ -> []
  where
  isNPArg arg = case arg of
    NPArg {} -> True
    _ -> False
  reorderArgs args = filter isNPArg args ++ filter (not . isNPArg) args
