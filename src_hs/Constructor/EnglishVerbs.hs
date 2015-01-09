module Constructor.EnglishVerbs (VerbForm(..), verb, haveForm, beForm, isGerund, auxVerbs, finiteVerb) where
import Constructor.Sense
import Constructor.Inference
import Constructor.ArgumentPlanning
import qualified Constructor.SemanticProperties as P
import Data.Maybe

data VerbForm = BaseVerb | Sg3Verb | PastVerb | Gerund deriving (Eq, Show)

verb verbForm frame = if isNothing (getType frame) then "???vp" else
  let negated = Just "true" == sValue P.Negated frame && not (Just "true" == (fValue P.Arg1 frame >>= sValue P.Negated)) in
  case fromJust $ getType frame of
  "copula" -> beForm (fValue P.Arg1 frame) (if sValue P.Time frame /= Just "PAST" then BaseVerb else verbForm)
  "copula_about" -> beForm (fValue P.Arg1 frame) (if sValue P.Time frame /= Just "PAST" then BaseVerb else verbForm)
  "ARGUE" -> if verbForm == Gerund then "arguing" else if Just "true" == sValue P.Irrealis frame then "were arguing" else "argue"
  "ARRIVE" -> "arrived"
  "ASK" -> if (fValue P.Topic frame >>= getType) == Just "PREDICAMENT" then if verbForm == PastVerb then "consulted" else "consult" else if verbForm == BaseVerb then "ask" else "asked"
  "BEGIN" -> "started"
  "BREAK" -> if verbForm == BaseVerb then "break" else "broke"
  "CAN" -> if negated then "couldn't" else "could"
  "COME_SCALARLY" -> if sValue P.Time frame == Just "PAST" then "went" else if verbForm == BaseVerb then "come" else "comes"
  "COUNT" -> if verbForm == Gerund then "counting" else "count"
  "DANCE" -> if verbForm == Gerund then "dancing" else "dance"
  "DISCOVER" -> "discovered"
  "DISPERSE" -> "went"
  "DISTRACT" -> "distracted"
  "DO" -> if verbForm == BaseVerb then "do" else "did"
  "FALL" -> "fell"
  "FALL_OUT" -> "fell out"
  "FORGET" -> "forgot"
  "GET_SAD" -> "got sad"
  "GO" -> if verbForm == PastVerb then "went" else if verbForm == BaseVerb then "go" else if verbForm == Gerund then "going" else "goes"
  "GO_OFF" -> "went"
  "HAPPEN" -> "happened"
  "HELP" -> "help"
  "KNOW" -> if verbForm == Sg3Verb then "knows" else "know"
  "LEAN_OUT" -> "looked out"
  "LOOK" -> "staring"
  "LOVE" -> if verbForm == BaseVerb then "love" else if negated then "doesn't love" else "loves"
  "MOVE" -> "moved"
  "NEED" -> "need"
  "RECALL" -> "recall"
  "REMEMBER" -> if verbForm == PastVerb then "remembered" else if verbForm == Sg3Verb then "remembers" else "remember"
  "RUN_OUT" -> "ran"
  "SAY" -> if isJust $ fValue P.Addressee frame
           then if verbForm == PastVerb then "told" else "tell"
           else if verbForm == PastVerb then "said" else "say"
  "SEE" -> if verbForm == BaseVerb then "see" else "saw"
  "SEEM" -> if isJust (usage P.Content frame >>= usage P.Reason) then
     if verbForm == PastVerb then "were" else "is"
     else if verbForm == PastVerb then "seemed" else "seems"
  "SIT" -> if verbForm == BaseVerb then "sit" else if verbForm == PastVerb then "sat" else "sitting"
  "SMASH" -> "smashed into the ground"
  "SMILE" -> "gave us a " ++ (if (fValue P.Manner frame >>= getType) == Just "SADLY" then "sad " else "") ++ "smile"
  "STOP" -> "stopped"
  "TAKE_OUT" -> "took"
  "THINK" -> if verbForm == BaseVerb then "think" else "thinking"
  "TO_ORDER" -> if verbForm == BaseVerb then "order" else "ordered"
  "TO_PRESENT" -> if verbForm == BaseVerb then "give" else "gave"
  "TO_WATER" -> if verbForm == Gerund then "watering" else "water"
  "THANK" -> "thanked"
  "TYPE" -> "typed"
  "WORK" -> "works"
  typ -> typ

beForm fSubject verbForm =
  if verbForm == PastVerb then
    if Just "Pl" == (fSubject >>= sValue P.RusNumber) then "were" else "was"
  else if Just "ME" == (fSubject >>= getType) then "am"
  else if Just "Pl" == (fSubject >>= sValue P.RusNumber) then "are" else "is"

haveForm fSubject fVerb verbForm =
  if verbForm == PastVerb then "had"
  else if Just True == fmap (hasAnyType ["ME", "WE"]) fSubject then "have"
  else "has"

auxVerbs fVerb fSubject verbForm isFuture isModality isDoModality isQuestion inverted isCopula itSubject =
  if isGerund fVerb && isNothing (usage P.Content fVerb >>= usage P.Member2) then beForm fSubject verbForm
  else if isFuture then "will"
  else if isModality && isQuestion then
    if isNothing fSubject then ""
    else if isDoModality then beForm fSubject verbForm
    else "should"
  else if hasType "copula_talking_about" fVerb then beForm fSubject PastVerb
  else if inverted then
    if isCopula then beForm fSubject verbForm
    else if verbForm == PastVerb then "did"
    else if Just True == fmap (hasAnyType ["ME", "THEY"]) fSubject then "do"
    else "does"
  else if itSubject then "is"
  else ""

finiteVerb fVerb fSubject verbForm isCopula inverted isFuture isModality isQuestion isDoModality thereSubject itSubject aux =
  if isEllipsisAnchor fSubject fVerb
  then
    if fSubject == (usage P.Content fVerb >>= fValue P.EllipsisAnchor2) then if verbForm == PastVerb then "did" else "does"
    else "-"
  else if hasType "copula_talking_about" fVerb then "talking"
  else if isCopula && inverted then ""
  else if isCopula && isFuture then "be"
  else if isModality then
    if isQuestion then if isJust fSubject && isDoModality then "supposed" else ""
    else if thereSubject then if verbForm == PastVerb then "was" else "is"
    else haveForm fSubject fVerb (if isFuture then BaseVerb else verbForm)
  else if itSubject then case fSubject >>= getType of
    Just "SNOW" -> (if isFuture then "be " else "") ++ "snowing"
    Just "RAIN" -> (if isFuture then "be " else "") ++ "raining"
    _ -> "WEATHER"
  else verb (if isGerund fVerb then Gerund else if null aux then verbForm else BaseVerb) fVerb

isGerund fVerb =
  Just "true" == sValue P.Imperfective fVerb ||
  hasType "SIT" fVerb ||
  hasType "THINK" fVerb && isNothing (fValue P.Topic fVerb)
