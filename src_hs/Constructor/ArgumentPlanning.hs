module Constructor.ArgumentPlanning (
  Argument(..), argumentFrame, arguments,
  isCP, isCPOrSeq, isFactCP, isQuestionCP,
  isVerbEllipsis, isEllipsisAnchor, reachesEllipsisAnchor,
  allCoordinatedVerbs,
  Position(..), argPosition,
  isAtLocationCopula, isExclamationCopula,
  lookAsWatching, pesterAsBoredGerund) where
import Constructor.Sense
import Constructor.Inference
import Constructor.Util
import Data.List
import qualified Data.Set as Set
import Data.Maybe
import Constructor.Variable
import Control.Applicative
import qualified Constructor.SemanticProperties as P
import qualified Constructor.LinkedSet as LS

isCPOrSeq frame = any isCP $ flatten frame

isVerbEllipsis verb = Just "true" == (usage P.Content verb >>= sValue P.Elided)

isEllipsisAnchor arg fVerb = isJust arg && (arg == (cp >>= fValue P.EllipsisAnchor1) || arg == (cp >>= fValue P.EllipsisAnchor2)) where
  cp = usage P.Content fVerb

reachesEllipsisAnchor mArg fVerb = case mArg of
  Nothing -> False
  Just arg -> any (\f -> isEllipsisAnchor (Just f) fVerb) $ Set.elems $ reachableFrames arg

data Position = BeforeCP | BeforeVP | BeforeVerb | AfterVerb deriving (Eq,Show,Ord)

data Argument = Adverb Position String | NPArg Frame |
                PPArg String Frame | PPAdjunct Position String Frame |
                ToInfinitive Frame | GerundBackground Position Frame |
                GerundArg Frame |
                Silence Frame |
                CommaSurrounded Argument |
                AccordingTo Frame Frame Position |
                PreAdverb String
                deriving (Eq,Show,Ord)

argumentFrame (NPArg f) = Just f
argumentFrame (PPArg _ f) = Just f
argumentFrame (ToInfinitive f) = Just f
argumentFrame (GerundBackground _ f) = Just f
argumentFrame (Silence f) = Just f
argumentFrame (AccordingTo f _ _) = Just f
argumentFrame (CommaSurrounded a) = argumentFrame a
argumentFrame _ = Nothing

argPosition (Adverb p _) = p
argPosition (PPAdjunct p _ _) = p
argPosition (GerundBackground p _) = p
argPosition (AccordingTo _ _ p) = p
argPosition (CommaSurrounded a) = argPosition a
argPosition _ = AfterVerb

arguments fVerb@(getType -> Just typ) = allArgs ++ externalArguments fVerb where
  sens = sense fVerb
  compareFacts f1@(Fact _ val1) f2@(Fact _ val2) =
    if f1 == f2 then EQ
    else case (val1, val2) of
      (VarValue _ v1, VarValue _ v2) |
        fVal1 <- Frame v1 sens,
        fVal2 <- Frame v2 sens,
        isJust (getType $ Frame v1 sens) && isJust (getType $ Frame v2 sens) ->
          if typeEarlier fVal1 fVal2 then LT else GT
      _ -> EQ
  sortedFacts = Data.List.sortBy compareFacts (allFrameFacts fVerb)
  allArgs = sortedFacts >>= \(Fact _ semValue) -> case semValue of
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
      ("COME_TO", P.Goal_by) -> [NPArg value]
      ("COME_TO", P.Domain) | isJust $ sValue P.Type value -> [PPArg "in" value]
      ("FORGET", P.Order) -> [Adverb AfterVerb "what comes next"]
      ("HAPPEN", P.Experiencer) -> [PPArg "to" value]
      ("TAKE_OUT", P.Source) -> [PPArg "out of" value]
      ("RUN_OUT", P.Source) -> [PPArg "out of" value]
      ("FALL", P.Source) -> [PPArg "off" value]
      ("SAY", P.Addressee) -> [NPArg value]
      ("ASK", P.Topic) ->
        if all isQuestionCP $ flatten value then []
        else [PPArg (if hasType "PREDICAMENT" value then "on" else "about") value]
      ("LACK", P.Theme) -> [NPArg value]
      ("DISTRACT", P.Theme) -> [PPArg "from" value]
      ("THINK", P.Theme) -> [PPArg "about" value]
      ("THINK", P.Topic) -> [PPArg "on" value]
      ("LET", P.Theme) -> [Adverb AfterVerb $ if hasType "SLEEP" value then "sleep" else "have a rest"]
      ("SEEM", P.Experiencer) -> if isJust (usage P.Content fVerb >>= usage P.Reason) then [] else [PPAdjunct AfterVerb "to" value]
      ("SEEM", P.Theme) ->
        if hasType "LACK" value then [PPArg "void of" (fromJust $ fValue P.Theme value)]
        else if hasType "MEANINGLESS" value then [Adverb AfterVerb "meaningless"]
        else if hasType "CLEVER" value then [Adverb AfterVerb "clever"]
        else [Adverb AfterVerb (fromMaybe "??" $ getType value)]
      ("DISPERSE", P.Goal) -> if hasType "HOUSES" value then [Adverb AfterVerb "home"] else [PPArg "to" value]
      ("GO", P.Goal_action) -> if hasType "WALK" value then [Adverb AfterVerb "for a walk"] else [PPArg "to" value]
      ("TYPE", P.Instrument) -> [PPArg "using" value]
      ("GIVE", P.Receiver) -> [PPArg "to" value]
      ("TO_PRESENT", P.Receiver) -> [PPArg "to" value]
      ("LOOK", P.Goal) -> if hasType "DOWN" value then [Adverb AfterVerb "down"] else [PPArg "at" value]
      ("LOOK", P.Goal_on) -> if lookAsWatching fVerb then [NPArg value] else [PPArg "at" value]
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
        Just _ -> [PPAdjunct AfterVerb "with" value]
        _ -> []
      (_, P.Location) | hasType "wh" value -> [NPArg value]
      (_, P.Location_on) -> [PPArg "on" value]
      (_, P.Location_in) -> [PPArg "in" value]
      (s, P.Location_at) | s /= "copula" -> [PPArg "next to" value]
      ("copula_about", P.Arg2) -> [PPArg "about" $ fromJust $ fValue P.Arg2 value]
      ("copula_talking_about", P.Arg2) -> [PPArg "about" $ fromJust $ fValue P.Arg2 value]
      ("copula", P.Arg1) | isOwnerCopula fVerb -> [NPArg value]
      ("copula", P.Arg2) | Just q <- fValue P.Quality value, hasType "placeholder" value && hasType "wh" q -> [NPArg q]
      ("PESTER", P.Arg2) -> []
      ("PESTER", P.Arg1) -> if pesterAsBoredGerund fVerb then [GerundArg value] else [PPArg "of" value]
      (_, P.Arg2) -> if isCPOrSeq value then [] else [NPArg value]
      (_, P.Duration) -> if hasType "LONG" value then [Adverb AfterVerb "for a long time"] else []
      (_, P.VTime) | hasType "wh" value -> [NPArg value]
      (_, P.RelTime) -> case fValue P.Anchor value of
        Just anchor -> [PPAdjunct AfterVerb (if hasType "AFTER" value then "after" else "before") anchor]
        _ -> let mod = if Just "ONLY" == sValue P.ModifierAdverb value then "just " else ""
                 prefix = typeEarlier value fVerb && Just True == fmap (typeEarlier value) (fValue P.Arg1 fVerb) || shouldContrastRelTime fVerb
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
      (_, P.Reason) | not (hasType "situation" value) ->
        if Just "but" == (fmap unSeq1 (usage P.Content fVerb) >>= usage P.Member2 >>= sValue P.Conj)
        then [PPAdjunct BeforeVP "out of" value]
        else [CommaSurrounded (PPAdjunct BeforeVP "because of" value)]
      _ -> []
    StrValue attr value -> case (attr, value) of
      (P.SAnchor, "AGAIN") -> [Adverb AfterVerb "again"]
      (P.SAnchor, "ALREADY") -> [Adverb AfterVerb "already"]
      (P.Also, "true") | typ /= "CAN" -> [Adverb BeforeVerb "also"]
      _ -> []
arguments _ = []

externalArguments fVerb = according ++ outOf where
  according = case fValue P.AccordingTo fVerb of
    Just acc -> [AccordingTo acc fVerb (if typeEarlier acc fVerb then BeforeVP else AfterVerb)]
    _ -> []
  outOf = case (getDeclaredType fVerb, fDeclaredValue P.Source fVerb, fValue P.Source fVerb) of
    (Just "FALL_OUT", Nothing, Just value) -> [PPArg "of" value]
    _ -> []

allCoordinatedVerbs fVerb = case usage P.Content fVerb of
  Nothing -> [fVerb]
  Just cp -> let
    allSenseCPs = findFrames "situation" (sense fVerb)
    i = fromJust $ elemIndex cp allSenseCPs
    windowSize = 5
    start = max (i - windowSize) 0
    window = take (2 * windowSize) $ drop start allSenseCPs
    sequences = LS.removeDups $ map unSeq window
    componentCPs = sequences >>= flatten
    in catMaybes $ map (fValue P.Content) componentCPs

shouldContrastRelTime fVerb = length (catMaybes $ map (fValue P.RelTime) $ allCoordinatedVerbs fVerb) > 1

isAtLocationCopula fVerb = hasType "copula" fVerb &&
  isJust (fValue P.Location_at fVerb) &&
  Just "SUCH" == (fValue P.Arg2 fVerb >>= fValue P.Determiner >>= getType)

isExclamationCopula fVerb = case (getType fVerb, fValue P.Arg1 fVerb, fValue P.Arg2 fVerb) of
  (Just "copula", Just arg1, Just arg2) | Just det2 <- fValue P.Determiner arg2 ->
    hasType "SUCH" det2 && typeEarlier det2 arg1 && typeEarlier arg1 arg2
  _ -> False

lookAsWatching fVerb = isTrue $ hasType "PESTER" <$> usage P.Arg1 fVerb

pesterAsBoredGerund fVerb = hasType "PESTER" fVerb && Just "LOOK" == (getType =<< fValue P.Arg1 fVerb)