module Constructor.ArgumentPlanning (Argument(..), argumentFrame, arguments, isCP, isFactCP, isQuestionCP, isVerbEllipsis, isEllipsisAnchor) where
import Constructor.Sense
import Data.List
import Data.Maybe
import Constructor.Variable
import qualified Constructor.SemanticProperties as P

isCPOrSeq frame = any isCP $ flatten $ Just frame

isVerbEllipsis verb = Just "true" == (usage P.Content verb >>= sValue P.Ellipsis)

isEllipsisAnchor arg fVerb = isJust arg && (arg == (cp >>= fValue P.EllipsisAnchor1) || arg == (cp >>= fValue P.EllipsisAnchor2)) where
  cp = usage P.Content fVerb

data Argument = Adverb String | NPArg Frame | PPArg String Frame | PPAdjunct String Frame deriving (Eq,Show)

argumentFrame (NPArg f) = Just f
argumentFrame (PPArg _ f) = Just f
argumentFrame _ = Nothing

arguments fVerb = reorderArgs $ fromMaybe [] $ flip fmap (getType fVerb) $ \typ -> let
  sens = sense fVerb
  compareFacts f1@(Fact frame1 val1) f2@(Fact frame2 val2) =
    if f1 == f2 then EQ
    else case (val1, val2) of
      (VarValue _ v1, VarValue _ v2) |
        fVal1 <- Frame v1 sens,
        fVal2 <- Frame v2 sens,
        isJust (getType $ Frame v1 sens) && isJust (getType $ Frame v2 sens) ->
          if typeEarlier fVal1 fVal2 then LT else GT
      _ -> EQ
  in Data.List.sortBy compareFacts (allFrameFacts fVerb) >>= \(Fact _ semValue) ->
  case semValue of
    VarValue attr v -> let value = Frame v sens in
     if isVerbEllipsis fVerb && not (isEllipsisAnchor (Just value) fVerb) then [] else
     case (typ, attr) of
      ("COME_SCALARLY", P.Order) -> case getType value of
        Just "EARLIER" -> case fValue P.Anchor value of
          Just anchor -> [PPArg "before" anchor]
          _ -> [Adverb "first"]
        Just "NEXT" -> [Adverb "next"]
        Just "AFTER" -> case fValue P.Anchor value of
          Just anchor -> [PPArg "after" anchor]
          _ -> [Adverb "after"]
        Just "BEFORE" -> case fValue P.Anchor value of
          Just anchor -> [PPArg "before" anchor]
          _ -> [Adverb "before"]
        _ -> []
      ("HAPPEN", P.Experiencer) -> [PPArg "to" value]
      ("TAKE_OUT", P.Source) -> [PPArg "out of" value]
      ("RUN_OUT", P.Source) -> [PPArg "out of" value]
      ("FALL", P.Source) -> [PPArg "off" value]
      ("SAY", P.Addressee) -> [NPArg value]
      ("ASK", P.Topic) ->
        if all isQuestionCP $ flatten $ Just value then []
        else [PPArg (if hasType "PREDICAMENT" value then "on" else "about") value]
      ("LACK", P.Theme) -> [NPArg value]
      ("DISTRACT", P.Theme) -> [PPArg "from" value]
      ("THINK", P.Topic) -> [PPArg "on" value]
      ("SEEM", P.Experiencer) -> if isJust (usage P.Content fVerb >>= usage P.Reason) then [] else [PPAdjunct "to" value]
      ("SEEM", P.Theme) ->
        if hasType "LACK" value then [PPArg "void of" (fromJust $ fValue P.Theme value)]
        else if hasType "MEANINGLESS" value then [Adverb "meaningless"]
        else if hasType "CLEVER" value then [Adverb "clever"]
        else [Adverb (fromMaybe "??" $ getType value)]
      ("DISPERSE", P.Goal) -> if hasType "HOUSES" value then [Adverb "home"] else [PPArg "to" value]
      ("GO", P.Goal_action) -> if hasType "WALK" value then [Adverb "for a walk"] else [PPArg "to" value]
      ("TYPE", P.Instrument) -> [PPArg "using" value]
      ("TO_PRESENT", P.Receiver) -> [PPArg "to" value]
      (_, P.Goal) -> if typ == "GO" && hasType "HOME" value then [Adverb "home"] else [PPArg "to" value]
      (_, P.Goal_to) -> [PPArg "to" value]
      (_, P.Goal_in) -> [PPArg "to" value]
      (_, P.Goal_on) -> [PPArg "to" value]
      (_, P.Source) -> [PPArg "from" value]
      (_, P.Instrument) -> [PPArg "with" value]
      (_, P.Mood) -> case getType value of
        Just "JOY" | isNothing (fValue P.Size value)-> [Adverb "cheerfully"]
        Just _ -> [PPAdjunct "with" value]
        _ -> []
      (_, P.Location) | hasType "wh" value -> [NPArg value]
      (_, P.Location) | not (hasType "THERE" value) -> [PPArg "on" value]
      (_, P.Arg2) -> if isCPOrSeq value then [] else [NPArg value]
      (_, P.Duration) -> if hasType "LONG" value then [Adverb "for a long time"] else []
      (_, P.VTime) | hasType "wh" value -> [NPArg value]
      (_, P.RelTime) -> case fValue P.Anchor value of
        Just anchor -> [PPAdjunct (if hasType "AFTER" value then "after" else "before") anchor]
        _ -> []
      _ -> []
    StrValue attr value -> case (attr, value) of
      (P.SAnchor, "AGAIN") -> [Adverb "again"]
      (P.SAnchor, "ALREADY") -> [Adverb "already"]
      _ -> []
  where
  isNPArg arg = case arg of
    NPArg {} -> True
    _ -> False
  reorderArgs args = filter isNPArg args ++ filter (not . isNPArg) args
