module Constructor.EnglishGenerator (generate) where
import Constructor.Sense
import Constructor.ArgumentPlanning
import Constructor.EnglishNouns
import Control.Monad.State
import Control.Monad
import Data.List
import Data.Char (toUpper)
import Data.Maybe
import qualified Data.Set as Set
import Constructor.Variable
import Constructor.Util
import Data.Function (on)
import qualified Constructor.SemanticProperties as P

data GenerationState = GenerationState { visitedFrames:: Set.Set Frame, past:: Bool}
data VerbForm = BaseVerb | Sg3Verb | PastVerb | Gerund deriving (Eq)

generate:: Sense -> String
generate sense =
  let topFrames = catMaybes $ map getTopFrame $ allFrames sense
      sentenceState = foldM generateSentence [] topFrames
      generateSentence :: String -> Frame -> State GenerationState String
      generateSentence output frame = do
        state <- get
        if Set.member frame (visitedFrames state) then return output
        else do
          nextSentence <- sentence frame
          let start = if (sValue P.DirectSpeech frame) == Just "true" then "- " else ""
              separator = if null output then "" else if start == "- " then "\n" else " "
          return $ output ++ separator ++ start ++ capitalize nextSentence
      text = evalState sentenceState $ GenerationState Set.empty False
  in stripLastComma text

capitalize (c:rest) = (toUpper c):rest

getTopFrame frame = if isCP frame then upmostSeq frame else Nothing where
  upmostSeq frame =
    case usage P.Member1 frame of
      Just p -> upmostSeq p
      _ -> if isJust $ usage P.Member2 frame then Nothing else Just frame

handleSeq :: (Frame -> State GenerationState String) -> Maybe Frame -> State GenerationState String
handleSeq _ Nothing = return "???seq"
handleSeq f (Just frame) =
  if hasType "seq" frame then do
      frameGenerated frame
      let first = fValue P.Member1 frame
          second = fValue P.Member2 frame
      m1 <- handleSeq f first
      state <- get
      let secondProcessed = Just True == fmap (flip Set.member (visitedFrames state)) second
      if secondProcessed then return m1 else do
        m2 <- handleSeq f second
        let conj = fromMaybe "" $ sValue P.Conj frame
            firstContent = first >>= fValue P.Content
            secondContent = second >>= fValue P.Content
            separator = if conj == "but" then
                          if Just "true" == (firstContent >>= sValue P.Irrealis) then ", when"
                          else if (Just True == fmap isGerund secondContent || Just True == fmap isGerund firstContent) &&
                                  isNothing (firstContent >>= sValue P.Negated)
                            then "and"
                          else if Just True == fmap shouldContrastSubject (firstContent >>= fValue P.Arg1) then ", and"
                          else ", but"
                        else if conj == "and" then
                          if Just True == fmap isCP second && Just True == fmap (hasType "seq") first then ", and"
                          else if isJust (secondContent >>= fValue P.PerfectBackground) then ", and"
                          else "and"
                        else if conj == "" then
                          if isJust (secondContent >>= fValue P.AccordingTo) then "; but"
                          else if isJust (secondContent >>= sValue P.AndEmphasis) then ""
                          else ","
                        else conj
        return $ m1 `cat` separator `cat` m2
  else f frame

np nom frame =
  if isSeq && (frame >>= fValue P.Member2 >>= getType) == Just "STREET" then
    handleSeq (return . streetName) frame `catM` return "streets"
  else if Just True == fmap (hasType "STREETS") frame then
    handleSeq (return . streetName) (frame >>= fValue P.Components) `catM` return "streets"
  else handleSeq (np_internal nom mayHaveDeterminer) frame where
  isSeq = (frame >>= getType) == Just "seq"
  isSubj = isJust (frame >>= usage P.Arg1)
  isAnchor = isJust (frame >>= usage P.Anchor)
  mayHaveDeterminer = if isNumber frame then isSubj || isAnchor || renderAsWord (fromJust frame) else True

np_internal :: Bool -> Bool -> Frame -> State GenerationState String
np_internal nom mayHaveDeterminer frame = do
  frameGenerated frame
  unquantified <- if hasType "ME" frame then if nom then return "I" else return "me"
    else if hasType "HE" frame then if nom then return "he" else return "him"
    else if hasType "SHE" frame then if nom then return "she" else return "her"
    else if hasType "THEY" frame then if nom then return "they" else return "them"
    else if hasType "WE" frame then if nom then return "we" else return "us"
    else if hasType "EVERYTHING" frame then return "everything"
    else if hasType "EVERYBODY" frame then return "everybody"
    else if hasType "wh" frame then return $
      if isJust (usage P.Goal frame) then "where"
      else if isJust (usage P.VTime frame) then "when"
      else if isJust (usage P.Location frame) then "where"
      else if isAnimate frame then
        if Just "true" == sValue P.Negated frame then "nobody" else if nom then "who" else "whom"
      else if isJust $ usage P.Questioned frame >>= usage P.Relative then "that"
      else "what"
    else do
      let n = if Just "true" == sValue P.Elided frame then
                if Just "Pl" == sValue P.RusNumber frame then "ones" else "one"
              else noun (getType frame) frame
          adjs = foldl cat "" $ adjectives frame
          nbar1 = adjs `cat` n
      nbar <- case getType frame of
         Just "ORDER" | Just poss <- fValue P.Arg1 frame -> handleSeq (np_internal True False) (Just poss) `catM` return nbar1
         Just "STREET" | not (prefixName frame) -> return nbar1 `catM` return (streetName frame)
         _ | Just loc <- fValue P.Location_on frame -> return nbar1 `catM` return "on" `catM` np False (Just loc)
         _ | Just src <- fValue P.Source frame -> return nbar1 `catM` return "from" `catM` np False (Just src)
         _ -> return nbar1
      genitiveComplement <- case getType frame of
        Just "CORNER" -> case fValue P.Arg1 frame of
          Just gen -> return "of" `catM` np False (Just gen)
          _ -> return ""
        Just "OPINION" | not $ isDeterminerOpinion frame -> case fValue P.Arg1 frame of
          Just gen -> return "of" `catM` elideableArgument (Just gen) frame
          _ -> return ""
        _ -> return ""
      det <- if mayHaveDeterminer then determiner frame nbar else return ""
      return $ det `cat` nbar `cat` genitiveComplement
  let fQuantifier = fValue P.Quantifier frame
  let allOf = if (fValue P.Specifier_all frame >>= getType) == Just "ALL" then "all of" else ""
  let postQuantifier = if not (null allOf) && hasType "WE" frame ||
                          (usage P.Arg1 frame >>= getType) == Just "DISPERSE"
                       then "all" else ""
  preQuantifier <- case fQuantifier >>= getType of
    Just "BOTH" -> return "both of"
    Just typ -> let q = handleSeq (np_internal True False) fQuantifier in
      if typ == "1" || isNothing (fValue P.Arg1 frame >>= getType) || any (\f -> fDeterminer frame == fDeterminer f) (prevSiblings frame)
      then q else return (if null allOf || not (null postQuantifier) then "" else "all") `catM` q `catM` return "of"
    _ -> return $ if null postQuantifier then allOf else ""
  relative <- case fValue P.Relative frame of
    Just relativeCp -> let rel = sentence relativeCp in
      if Just "copula" == (fValue P.Content relativeCp >>= getType) then return ", the one" `catM` rel
      else rel
    _ -> return ""
  return $ preQuantifier `cat` unquantified `cat` postQuantifier `cat` relative

adjectives nounFrame = catMaybes [property, kind, shopKind, size, quality, gender] where
  property = fValue P.Property nounFrame >>= getType >>= \p -> if p == "AMAZING" then Just "amazing" else Nothing
  kind = fValue P.Kind nounFrame >>= getType >>= \p -> if p == "COMMERCIAL" then Just "commercial" else Nothing
  quality = fValue P.Quality nounFrame >>= getType >>= \case
    "HUMBLE" -> Just "humble"
    "CLEVER" -> Just "smart"
    "STUPID" -> Just "stupid"
    _ -> Nothing
  shopKind = sValue P.Name nounFrame >>= \p -> if p == "гастроном" then Just "grocery" else Nothing
  gender =
    if shouldContrastSubject nounFrame && isHuman nounFrame
    then case sValue P.RusGender nounFrame of
      Just "Masc" -> Just "male"
      Just "Fem" -> Just "female"
      _ -> Nothing
    else Nothing
  size = fValue P.Size nounFrame >>= getType >>= \p ->
    if p == "LITTLE" then Just "small"
    else if p == "BIG" then
      if hasType "GARDEN" nounFrame then Just "big" else Just "great"
    else Nothing

shouldContrastSubject frame = let
  cp = usage P.Arg1 frame >>= usage P.Content
  allCPs = flatten (fmap unSeq cp)
  allVerbs = catMaybes $ map (fValue P.Content) allCPs
  contrastibleSubject fVerb = case fValue P.Arg1 fVerb of
    Just fSubj | not (isNumber $ Just fSubj), Just g1 <- sValue P.RusGender fSubj, Just g2 <- sValue P.RusGender frame, g1 /= g2 ->
      not (isVerbEllipsis fVerb) || isEllipsisAnchor (Just fSubj) fVerb
    _ -> False
  in any contrastibleSubject allVerbs

streetName frame = case sValue P.Name frame of
 Just "знаменская" -> "Znamenskaya"
 Just "бассейная" -> "Basseinaya"
 Just "театральная" -> "Teatralnaya"
 Just s -> s
 _ -> ""

fDeterminer frame =
  if hasAnyType ["NEIGHBORS", "AMAZE", "PREDICAMENT", "MOUTH", "NOSE", "JAW", "JAWS", "ARGUE", "FINGER", "SPEECH", "FAMILY", "EYES"] frame then fValue P.Arg1 frame
  else if hasAnyType ["OPINION"] frame && isDeterminerOpinion frame then fValue P.Arg1 frame
  else if hasAnyType ["WORDS", "BOOK"] frame then fValue P.Author frame
  else if hasAnyType ["ROOMS", "APARTMENTS", "OFFICES"] frame then fValue P.Owner frame
  else if hasAnyType ["CASHIER"] frame then fValue P.Place frame
  else Nothing

isDeterminerOpinion frame = unSeq frame == frame && all (hasAnyType ["ME", "THEY", "HE", "SHE", "wh"]) (flatten $ fValue P.Arg1 frame)
determiner frame nbar =
  let det = fDeterminer frame
      genitiveSpecifier det =
        case getType det of
          Just "ME" -> return "my"
          Just "HE" -> return "his"
          Just "THEY" -> return "their"
          Just "WE" -> return "our"
          Just "SHE" -> return "her"
          Just "wh" -> return "whose"
          Just s -> do
            state <- get
            if Set.member det (visitedFrames state) then return $ case sValue P.RusGender det of
               Just "Masc" -> "his"
               Just "Fem" -> "her"
               Just "Neu" -> "its"
               _ -> s
            else do
              let human = isHuman det
              sDet <- np_internal False (not human) det
              return $ if human then if "s" `isSuffixOf` sDet then sDet ++ "'" else sDet ++ "'s" else sDet
          _ -> return "???det"
  in
  case det of
    Just _det  | not $ any (\f -> fDeterminer frame == fDeterminer f) (prevSiblings frame) ->
      let own = if hasType "SELF" _det && hasType "EYES" frame && isNothing (fValue P.Quantifier frame) then "own" else ""
      in handleSeq genitiveSpecifier (fmap resolve det) `catM` return own
    _ -> return $
      let sDet = fValue P.Determiner frame >>= getType in
      if sDet == Just "THIS" then "this"
      else if sDet == Just "ANY" then "any"
      else if sDet == Just "wh" then "which"
      else if isJust (fValue P.Quantifier frame) then ""
      else if hasType "STREET" frame && prefixName frame then streetName frame
      else if hasAnyType ["SOME", "OTHERS", "THIS", "THAT", "JOY", "RELIEF", "MEANING", "MONEY", "COUNTING", "APARTMENTS", "OFFICES", "HOUSES"] frame then ""
      else if hasAnyType ["NAMED_PERSON"] frame then ""
      else if hasType "OPINION" frame && Just True == fmap isVerbEllipsis (usage P.AccordingTo frame) then ""
      else if sValue P.Given frame == Just "true" then "the"
      else if "a" `isPrefixOf` nbar || "e" `isPrefixOf` nbar || "8" `isPrefixOf` nbar then "an"
      else if isSingular frame then "a"
      else ""

prefixName frame = earlier frame P.Name frame P.Type

cat "" t2 = t2
cat t1 "" = t1
cat t1 t2 = case t2 of
 [] -> t1
 c:_ -> if c `elem` ",.:;?" then stripLastComma t1 ++ t2 else t1 ++ " " ++ t2

stripLastComma t1 = if "," `isSuffixOf` t1 then take (length t1 - 1) t1 else t1

catM :: State GenerationState String -> State GenerationState String -> State GenerationState String
catM t1 t2 = do s1 <- t1; s2 <- t2; return $ s1 `cat` s2

frameGenerated frame = do state <- get; put $ state { visitedFrames = Set.insert frame $ visitedFrames state }

sentence :: Frame -> State GenerationState String
sentence frame = handleSeq singleSentence (Just frame) `catM` return finish where
  singleSentence frame = do
    frameGenerated frame
    fromMaybe (return "???sentence") $ liftM clause $ fValue P.Content frame
  finish = if sValue P.Dot frame == Just "true" then "."
           else if sValue P.Question_mark frame == Just "true" then "?"
           else case lastSentence >>= fValue P.Content >>= fValue P.Message of
             Just message -> if isNothing (getType message) then ":" else ""
             _ -> ""
  lastSentence = if hasType "seq" frame then fValue P.Member2 frame else Just frame

genComplement cp = case fValue P.Content cp of
  Nothing -> return ""
  Just fVerb -> let
      prefix = if isFactCP cp && distinguish cp then "that" else ""
      negation = if Just "true" == sValue P.Negated cp then "not" else ""
    in return (negation `cat` prefix) `catM` sentence cp

isGerund fVerb =
  Just "true" == sValue P.Imperfective fVerb ||
  hasType "SIT" fVerb ||
  hasType "THINK" fVerb && isNothing (fValue P.Topic fVerb)

verb verbForm frame = if isNothing (getType frame) then "???vp" else
  let negated = Just "true" == sValue P.Negated frame && not (Just "true" == (fValue P.Arg1 frame >>= sValue P.Negated)) in
  case fromJust $ getType frame of
  "copula" -> beForm (fValue P.Arg1 frame) (if sValue P.Time frame /= Just "PAST" then BaseVerb else verbForm)
  "ARGUE" -> if verbForm == Gerund then "arguing" else if Just "true" == sValue P.Irrealis frame then "were arguing" else "argue"
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
  "FORGET" -> "forgot"
  "GET_SAD" -> "got sad"
  "GO" -> if verbForm == PastVerb then "went" else if verbForm == BaseVerb then "go" else if verbForm == Gerund then "going" else "goes"
  "GO_OFF" -> "went"
  "HAPPEN" -> "happened"
  "HELP" -> "help"
  "KNOW" -> if verbForm == Sg3Verb then "knows" else "know"
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
  "SMILE" -> "gave us a " ++ (if (fValue P.Manner frame >>= getType) == Just "SADLY" then "sad " else "") ++ "smile"
  "STOP" -> "stopped"
  "TAKE_OUT" -> "took"
  "THINK" -> if verbForm == BaseVerb then "think" else "thinking"
  "TO_ORDER" -> if verbForm == BaseVerb then "order" else "ordered"
  "TO_PRESENT" -> if verbForm == BaseVerb then "give" else "gave"
  "TO_WATER" -> if verbForm == Gerund then "watering" else "water"
  "THANK" -> "thanked"
  "TYPE" -> "typed"
  typ -> typ

conjIntroduction fVerb =
   if sValue P.ButEmphasis fVerb == Just "true" then "but"
   else if sValue P.AndEmphasis fVerb == Just "true" then "and"
   else ""

distinguish frame = isNothing (usage P.Member2 frame) || Just "true" == sValue P.Distinguished frame ||
  hasType "OPINION" frame && Just "WORDS" == (usage P.Member2 frame >>= fValue P.Member1 >>= getType)

elideableArgument frame parent = if any (\f -> source f == frame) (nextSiblings parent) then return "" else np False frame where
  source f = if hasType "WORDS" f then fValue P.Author f else fValue P.Arg1 f

generateAccording parent = case fValue P.AccordingTo parent of
  Just source -> do
    let isWh = isQuestioned source
        comma = if isEllipsisAnchor (fValue P.Arg1 parent) parent || isWh then "" else ","
        oneOpinion source = case getType source of
          Just "OPINION" -> return (if distinguish source then "in" else "") `catM` np False (Just source)
          Just "WORDS" -> return "according to" `catM` elideableArgument (fValue P.Author source) source
          s -> return $ show s
    state <- get
    if Set.member source (visitedFrames state) then return ""
    else handleSeq oneOpinion (Just source) `catM` return comma
  _ -> return ""

clause :: Frame -> State GenerationState String
clause fVerb = do
    frameGenerated fVerb
    state <- get
    when (sValue P.Time fVerb == Just "PAST") (put $ state { past = True })
    state <- get
    let intro = conjIntroduction fVerb
    let emphasis = if (fValue P.OptativeModality fVerb >>= getType) == Just "LUCK" then "by some sheer luck,"
                  else if sValue P.Emphasis fVerb == Just "true" then "there"
                   else if (fValue P.RelTime fVerb >>= getType) == Just "AFTER" && isNothing (fValue P.RelTime fVerb >>= fValue P.Anchor) then "then"
                   else if (fValue P.RelTime fVerb >>= getType) == Just "BEFORE" && isNothing (fValue P.RelTime fVerb >>= fValue P.Anchor) then "before,"
                   else ""
    let verbForm = if past state then PastVerb else if Just True == fmap (hasAnyType ["ME", "WE", "THEY"]) fSubject then BaseVerb else Sg3Verb
        isModality = hasType "modality" fVerb
        isRaising = hasType "SEEM" fVerb
        fSubject = if isModality || isRaising then fValue P.Theme fVerb >>= fValue P.Arg1 else fValue P.Arg1 fVerb
        cp = usage P.Content fVerb
    core <- if hasType "degree" fVerb && (fromMaybe False $ fmap (hasType "wh") $ fValue P.Arg2 fVerb)
           then return "Great was" `catM` np True fSubject
           else if hasType "copula" fVerb && isJust (fValue P.Owner fVerb) then do
             let owner = fValue P.Owner fVerb
             subj <- np True owner
             let verb = haveForm owner fVerb verbForm
             obj <- np False (fValue P.Arg1 fVerb)
             return $ subj `cat` verb `cat` obj
           else vp fVerb verbForm FiniteClause
    elaboration <- case fValue P.Elaboration fVerb of
      Just smth -> return (if hasType "HAPPEN" fVerb then "," else ":") `catM` sentence smth
      _ -> return ""
    let fComp = case getType fVerb of
          Just "FORGET" -> fValue P.Arg2 fVerb
          Just "ASK" -> fValue P.Topic fVerb
          Just "DISCOVER" -> fValue P.Theme fVerb
          Just "SAY" -> case fValue P.Message fVerb of
            Just comp | isJust (getType comp) -> Just comp
            _ -> Nothing
          _ -> Nothing
    background <- case fValue P.PerfectBackground fVerb of
      Just back -> case getType back of
        Just "MOVE" -> do
          let slightly = if Just "SLIGHTLY" == (fValue P.Manner back >>= getType) then "slightly" else ""
          moved <- np False (fValue P.Arg2 back)
          return $ "moving" `cat` moved `cat` slightly `cat` "back and forth"
        Just "THINK" -> return "thinking carefully about" `catM` np False (fValue P.Theme back)
        Just "COME_TO" ->
          let domain = case fValue P.Domain back of
                         Just dom | isJust (sValue P.Type dom) -> return "in" `catM` np False (Just dom)
                         _ -> return ""
          in return (conjIntroduction back `cat` "reaching") `catM` np False (fValue P.Goal_by back) `catM` domain
        _ -> return ""
      _ -> return ""
    comp <- case fComp of
      Nothing -> return "" 
      Just cp -> let compVerb = fValue P.Content cp in
        if isQuestionCP cp && Just True == fmap (hasType "THINK") compVerb then
           do
             frameGenerated cp
             (return "about their opinion on") `catM` (np False $ fValue P.Topic $ fromJust compVerb)
        else let comma = if not (hasType "SAY" fVerb) && isFactCP (head $ flatten fComp) then "," else ""
             in return comma `catM` handleSeq genComplement fComp
    externalComp <- if getType fVerb == Just "GO" then 
      case cp >>= usage P.Member1 >>= fValue P.Member2 of
       Just nextClause | (fValue P.Content nextClause >>= getType) == Just "ASK" -> do
         frameGenerated nextClause
         return "to" `catM` vp (fromJust $ fValue P.Content nextClause) BaseVerb InfiniteClause
       _ -> return ""
      else return ""
    controlledComp <-
      if hasType "TO_ORDER" fVerb then
        case fValue P.Theme fVerb of
          Just slave -> return "to" `catM` vp slave BaseVerb InfiniteClause
          _ -> return ""
      else return ""
    condComp <- case fValue P.WhenCondition fVerb of
      Just fComp -> return ", when" `catM` sentence fComp
      _ -> case fValue P.IfCondition fVerb of
        Just fComp -> return ", if" `catM` sentence fComp
        _ -> case fValue P.Condition fVerb of
          Just caze | hasType "CASE" caze -> case msum [fValue P.WhenCondition caze, fValue P.IfCondition caze] of
            Just fComp -> do
              comp <- sentence fComp
              according <- generateAccording caze
              return $ ", only if" `cat` (if null according then "" else ", " ++ according) `cat` comp
            _ -> return ""
          _ -> return ""
    reasonComp <- case fValue P.Reason fVerb of
      Just fComp -> return "because" `catM` sentence fComp
      _ -> return ""
    questionVariants <- case cp >>= fValue P.Questioned >>= fValue P.Variants of
      Just variants -> return "-" `catM` np ((cp >>= fValue P.Questioned) == fSubject) (Just variants)
      _ -> return ""
    let coreWithBackground =
          if null background then core
          else if typeEarlier fVerb (fromJust $ fValue P.PerfectBackground fVerb) then core `cat` "," `cat` background `cat` ","
          else (if null emphasis then "" else ",") `cat` background `cat` "," `cat` core
    according <- generateAccording fVerb
    return $ intro `cat` emphasis `cat` according `cat` coreWithBackground `cat` controlledComp `cat` condComp `cat` reasonComp `cat` comp `cat` externalComp `cat` questionVariants `cat` elaboration

isQuestioned frame = flip any (flatten $ Just frame) $ \frame ->
  hasType "wh" frame ||
  Just True == fmap isQuestioned (fValue P.Arg1 frame) ||
  Just True == fmap isQuestioned (fValue P.Author frame) ||
  Just True == fmap isQuestioned (fValue P.Determiner frame)

beForm fSubject verbForm =
  if verbForm == PastVerb then
    if Just "Pl" == (fSubject >>= sValue P.RusNumber) then "were" else "was"
  else if Just "ME" == (fSubject >>= getType) then "am"
  else if Just "Pl" == (fSubject >>= sValue P.RusNumber) then "are" else "is"

haveForm fSubject fVerb verbForm =
  if verbForm == PastVerb then "had"
  else if Just "FUTURE" == sValue P.Time fVerb then "will have"
  else if Just True == fmap (hasAnyType ["ME", "WE"]) fSubject then "have"
  else "has"

data ClauseType = FiniteClause | InfiniteClause deriving (Eq)

vp :: Frame -> VerbForm -> ClauseType -> State GenerationState String
vp fVerb verbForm clauseType = do
  let preAdverb = case fValue P.Manner fVerb >>= getType of
        Just "SUDDENLY" -> "suddenly"
        Just "JUST" -> "just"
        Just "SADLY" -> if getType fVerb == Just "SMILE" then "" else "sadly"
        Just "SLIGHTLY" -> if getType fVerb == Just "MOVE" then "" else "slightly"
        Just s -> s
        _ -> ""
      cp = usage P.Content fVerb
      theme = fValue P.Theme fVerb
      isFuture = Just "FUTURE" == sValue P.Time fVerb
      isModality = hasType "modality" fVerb
      isRaising = hasType "SEEM" fVerb
      fSubject = if isModality || isRaising then theme >>= fValue P.Arg1 else fValue P.Arg1 fVerb
      isQuestion = Just True == fmap isQuestionCP cp
      nonSubjectQuestion = isQuestion && (cp >>= fValue P.Questioned) /= fSubject
      inverted = nonSubjectQuestion && Just "true" == (cp >>= sValue P.Question_mark)
      isDoModality = isModality && Just True == fmap (hasType "DO") theme
      thereSubject = clauseType == FiniteClause && (Just "wh" == (fSubject >>= getType) && isModality || isNothing (fSubject >>= getType)) && not isQuestion
      sVerb = if isEllipsisAnchor fSubject fVerb
              then
                if fSubject == (cp >>= fValue P.EllipsisAnchor2) then if verbForm == PastVerb then "did" else "does"
                else "-"
              else if isModality then
                if isQuestion then if isJust fSubject && isDoModality then "supposed" else ""
                else if thereSubject then if verbForm == PastVerb then "was" else "is"
                else haveForm fSubject fVerb verbForm
              else verb (if isGerund fVerb then Gerund else if null aux then verbForm else BaseVerb) fVerb
      finalAdverb = case getType fVerb of
        Just "HAPPEN" -> "today"
        Just "MOVE" -> (if Just "SLIGHTLY" == (fValue P.Manner fVerb >>= getType) then "slightly" else "") `cat` "back and forth"
        _ -> ""
      negation = if sValue P.Negated fVerb == Just "true" && isGerund fVerb then "not" else ""
      aux =
        if isGerund fVerb && isNothing (usage P.Content fVerb >>= usage P.Member2) then beForm fSubject verbForm
        else if isModality && isQuestion then
          if isNothing fSubject then ""
          else if isDoModality then beForm fSubject verbForm
          else "should"
        else if inverted then if verbForm == PastVerb then "did" else if Just True == fmap (hasAnyType ["ME", "THEY"]) fSubject then "do" else "does"
        else ""
      allArgs = if isModality then fromMaybe [] (fmap arguments theme) else arguments fVerb
      topicalizedArg = case (fSubject, allArgs) of
        (Just subj, hd@(PPAdjunct _ value):_) | typeEarlier value fVerb && typeEarlier value subj -> Just hd
        _ -> Nothing
      questionedArg = if not nonSubjectQuestion then Nothing else Data.List.find isQuestionedArg allArgs
      existentialWhArg =
        if isQuestion then Nothing
        else if clauseType == FiniteClause && Just True == fmap isQuestioned fSubject && isModality then Just (NPArg $ fromJust fSubject)
        else Data.List.find isQuestionedArg allArgs
      isQuestionedArg arg = Just True == fmap isQuestioned (argumentFrame arg)
      removeMaybe maybeVal list = fromMaybe list $ fmap (flip Data.List.delete list) maybeVal
      normalArgs = removeMaybe questionedArg $ removeMaybe topicalizedArg $ removeMaybe existentialWhArg $ allArgs
      stranded = case mplus questionedArg existentialWhArg of
        Just (PPArg prep val) -> if isJust (usage P.Goal val) then "" else prep
        _ -> ""
      anymore = let
        isAnymore = Just "true" == (existentialWhArg >>= argumentFrame >>= sValue P.Not_anymore) ||
                    clauseType == FiniteClause && Just "wh" == (fSubject >>= getType) && Just "true" == (fSubject >>= sValue P.Not_anymore) ||
                    Just "true" == sValue P.Not_anymore fVerb
        in if isAnymore then "anymore" else ""
  subject <- if thereSubject then return "there" else case (fSubject, clauseType) of
    (Just f, FiniteClause) ->
      if thereSubject then return "there"
      else if isVerbEllipsis fVerb && not (isEllipsisAnchor fSubject fVerb) then return "it"
      else if [fVerb] `isPrefixOf` (usages P.Arg1 f) || isModality || isRaising then np True fSubject
      else if (isJust $ fValue P.PerfectBackground fVerb) then return $ if sValue P.RusGender f == Just "Masc" then "he" else "she"
      else return ""
    _ -> return ""
  sArgs <- foldM (\s arg -> return s `catM` generateArg arg) "" $ Data.List.sortBy (compare `on` argOrder) normalArgs
  sTopicalized <- case topicalizedArg of
    Just arg -> generateArg arg `catM` return ","
    _ -> return ""
  whWord <- case questionedArg >>= argumentFrame of
    Just qFrame -> np (hasType "wh" qFrame) (Just qFrame)
    _ -> return ""
  nonWhWord <- case existentialWhArg >>= argumentFrame of
    Just frame -> return $
      if Just "true" == sValue P.Negated frame then
        if Just "true" == sValue P.Animate frame then "nobody" else if isJust $ usage P.Goal frame then "nowhere" else "nothing"
      else if Just "true" == sValue P.Animate frame then "somebody" else "something"
    _ -> return ""
  according <- if null whWord && Just True /= fmap (hasType "wh") fSubject then return "" else do
    acc <- generateAccording fVerb
    return $ if null acc then "" else "," `cat` acc
  controlled <- case getType fVerb of
    Just "CAN" -> case theme of
      Just slave -> vp slave BaseVerb InfiniteClause
      _ -> return ""
    Just "modality" -> case theme of
       Just slave -> return $ (if isDoModality || not isQuestion || isNothing fSubject then "to" else "") `cat` verb BaseVerb slave
       _ -> return ""
    Just "BEGIN" -> case theme of
      Just slave -> vp slave Gerund InfiniteClause
      _ -> return ""
    _ -> return ""
  let contracted = if null preAdverb && null negation && null aux && null according then
                     if sVerb == "am" then subject ++ "'m"
                     else if sVerb == "is" then subject ++ "'s"
                     else if "will " `isPrefixOf` sVerb then subject ++ "'ll" ++ drop (length "will") sVerb
                     else subject `cat` sVerb
                   else (if inverted then according `cat` aux `cat` negation `cat` subject else subject `cat` according `cat` aux `cat` negation) `cat` preAdverb `cat` sVerb
  return $ sTopicalized `cat` whWord `cat` contracted `cat` nonWhWord `cat` controlled `cat` sArgs `cat` stranded `cat` anymore `cat` finalAdverb

generateArg :: Argument -> State GenerationState String
generateArg arg = case arg of
  Adverb s -> return s
  NPArg f -> np False $ Just f
  PPArg prep f  -> if isJust (getType f) then return prep `catM` (np False $ Just f) else return ""
  PPAdjunct prep f -> return prep `catM` (np False $ Just f)

argOrder arg = case arg of
  PPAdjunct {} -> 1
  _ -> 0