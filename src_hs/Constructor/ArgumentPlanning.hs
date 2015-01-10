module Constructor.ArgumentPlanning (
  Argument(..), argumentFrame, arguments,
  isCP, isCPOrSeq, isFactCP, isQuestionCP,
  isVerbEllipsis, isEllipsisAnchor,
  Position(..), argPosition) where
import Constructor.Sense
import Constructor.Inference
import Data.List
import Data.Maybe
import Constructor.Variable
import qualified Constructor.SemanticProperties as P
import qualified Constructor.LinkedSet as LS

isCPOrSeq frame = any isCP $ flatten $ Just frame

isVerbEllipsis verb = Just "true" == (usage P.Content verb >>= sValue P.Ellipsis)

isEllipsisAnchor arg fVerb = isJust arg && (arg == (cp >>= fValue P.EllipsisAnchor1) || arg == (cp >>= fValue P.EllipsisAnchor2)) where
  cp = usage P.Content fVerb

data Position = BeforeVP | BeforeVerb | AfterVerb deriving (Eq,Show,Ord)

data Argument = Adverb Position String | NPArg Frame |
                PPArg String Frame | PPAdjunct String Frame |
                ToInfinitive Frame | GerundBackground Position Frame |
                Silence Frame |
                PreAdverb String
                deriving (Eq,Show,Ord)

argumentFrame (NPArg f) = Just f
argumentFrame (PPArg _ f) = Just f
argumentFrame (ToInfinitive f) = Just f
argumentFrame (GerundBackground _ f) = Just f
argumentFrame (Silence f) = Just f
argumentFrame _ = Nothing

argPosition (Adverb p _) = p
argPosition (GerundBackground p _) = p
argPosition _ = AfterVerb

arguments fVerb@(getType -> Just typ) = allArgs where
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
  sortedFacts = Data.List.sortBy compareFacts (allFrameFacts fVerb)
  allArgs = Data.List.sortBy compareFacts (allFrameFacts fVerb) >>= \(Fact _ semValue) -> case semValue of
    VarValue attr v -> let value = Frame v sens in
     if isVerbEllipsis fVerb && not (isEllipsisAnchor (Just value) fVerb) then [] else
     case (typ, attr) of
      ("COME_SCALARLY", P.Order) -> case getType value of
        Just "EARLIER" -> case fValue P.Anchor value of
          Just anchor -> [PPArg "before" anchor]
          _ -> [Adverb AfterVerb "first"]
        Just "NEXT" -> [Adverb AfterVerb "next"]
        Just "AFTER" -> case fValue P.Anchor value of
          Just anchor -> [PPArg "after" anchor]
          _ -> [Adverb AfterVerb "after"]
        Just "BEFORE" -> case fValue P.Anchor value of
          Just anchor -> [PPArg "before" anchor]
          _ -> [Adverb AfterVerb "before"]
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
        else if hasType "MEANINGLESS" value then [Adverb AfterVerb "meaningless"]
        else if hasType "CLEVER" value then [Adverb AfterVerb "clever"]
        else [Adverb AfterVerb (fromMaybe "??" $ getType value)]
      ("DISPERSE", P.Goal) -> if hasType "HOUSES" value then [Adverb AfterVerb "home"] else [PPArg "to" value]
      ("GO", P.Goal_action) -> if hasType "WALK" value then [Adverb AfterVerb "for a walk"] else [PPArg "to" value]
      ("TYPE", P.Instrument) -> [PPArg "using" value]
      ("TO_PRESENT", P.Receiver) -> [PPArg "to" value]
      ("LOOK", P.Goal) -> if hasType "DOWN" value then [Adverb AfterVerb "down"] else [PPArg "at" value]
      ("LOOK", P.Goal_on) -> [PPArg "at" value]
      (_, P.Goal) -> if typ == "GO" && hasType "HOME" value then [Adverb AfterVerb "home"] else [PPArg "to" value]
      (_, P.Goal_to) -> [PPArg "to" value]
      (_, P.Goal_in) -> [PPArg "to" value]
      (_, P.Goal_on) -> [PPArg "to" value]
      ("LEAN_OUT", P.Source) -> [PPArg "of" value]
      ("FALL_OUT", P.Source) -> [PPArg "of" value]
      (_, P.Source) -> [PPArg "from" value]
      (_, P.Instrument) -> [PPArg "with" value]
      (_, P.Mood) -> case getType value of
        Just "JOY" | isNothing (fValue P.Size value)-> [Adverb AfterVerb "cheerfully"]
        Just _ -> [PPAdjunct "with" value]
        _ -> []
      (_, P.Location) | hasType "wh" value -> [NPArg value]
      (_, P.Location_on) -> [PPArg "on" value]
      (_, P.Color) -> [Adverb AfterVerb "green"]
      (_, P.Location_in) -> [PPArg "in" value]
      (_, P.Location_at) -> [PPArg "next to" value]
      ("copula_about", P.Arg2) -> [PPArg "about" value]
      ("copula_talking_about", P.Arg2) -> [PPArg "about" value]
      (_, P.Arg2) -> if isCPOrSeq value then [] else [NPArg value]
      (_, P.Duration) -> if hasType "LONG" value then [Adverb AfterVerb "for a long time"] else []
      (_, P.VTime) | hasType "wh" value -> [NPArg value]
      (_, P.RelTime) -> case fValue P.Anchor value of
        Just anchor -> [PPAdjunct (if hasType "AFTER" value then "after" else "before") anchor]
        _ -> let mod = if Just "ONLY" == sValue P.ModifierAdverb value then "just " else ""
                 prefix = typeEarlier value fVerb && Just True == fmap (typeEarlier value) (fValue P.Arg1 fVerb)
                 wrap adverb = [Adverb (if prefix then BeforeVP else AfterVerb) $ mod ++ adverb]
             in
             if hasType "YESTERDAY" value then wrap "yesterday"
             else if hasType "TODAY" value then wrap "today"
             else if hasType "TOMORROW" value then wrap "tomorrow"
             else []
      (_, P.Manner) -> case getType value of
        Just "SUDDENLY" -> [Adverb BeforeVerb "suddenly"]
        Just "JUST" -> [Adverb BeforeVerb "just"]
        Just "SADLY" -> if typ == "SMILE" then [] else [Adverb BeforeVerb "sadly"]
        Just "SLIGHTLY" -> if typ == "MOVE" then [] else [Adverb BeforeVerb "slightly"]
        Just s -> [Adverb BeforeVerb s]
        _ -> []
      (_, P.PerfectBackground) -> [GerundBackground (if typeEarlier value fVerb then BeforeVP else AfterVerb) value]
      _ -> []
    StrValue attr value -> case (attr, value) of
      (P.SAnchor, "AGAIN") -> [Adverb AfterVerb "again"]
      (P.SAnchor, "ALREADY") -> [Adverb AfterVerb "already"]
      (P.Also, "true") | typ /= "CAN" -> [Adverb BeforeVerb "also"]
      _ -> []
arguments _ = []