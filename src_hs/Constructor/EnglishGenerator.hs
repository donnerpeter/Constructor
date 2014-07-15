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
          let start = if (sValue "directSpeech" frame) == Just "true" then "- " else ""
              separator = if null output then "" else if start == "- " then "\n" else " "
          return $ output ++ separator ++ start ++ capitalize nextSentence
      text = evalState sentenceState $ GenerationState Set.empty False
  in stripLastComma text

capitalize (c:rest) = (toUpper c):rest

getTopFrame frame = if isCP frame then upmostSeq frame else Nothing where
  upmostSeq frame =
    case usage "member1" frame of
      Just p -> upmostSeq p
      _ -> if isJust $ usage "member2" frame then Nothing else Just frame

handleSeq :: (Frame -> State GenerationState String) -> Maybe Frame -> State GenerationState String
handleSeq _ Nothing = return "???seq"
handleSeq f (Just frame) =
  if hasType "seq" frame then do
      frameGenerated frame
      let first = fValue "member1" frame
          second = fValue "member2" frame
      m1 <- handleSeq f first
      state <- get
      let secondProcessed = Just True == fmap (flip Set.member (visitedFrames state)) second
      if secondProcessed then return m1 else do
        m2 <- handleSeq f second
        let conj = fromMaybe "" $ sValue "conj" frame
            firstContent = first >>= fValue "content"
            secondContent = second >>= fValue "content"
            separator = if conj == "but" then
                          if Just "true" == (firstContent >>= sValue "irrealis") then ", when"
                          else if (Just True == fmap isGerund secondContent || Just True == fmap isGerund firstContent) &&
                                  isNothing (firstContent >>= sValue "negated")
                            then "and"
                          else if Just True == fmap shouldContrastSubject (firstContent >>= fValue "arg1") then ", and"
                          else ", but"
                        else if conj == "and" then
                          if Just True == fmap isCP second && Just True == fmap (hasType "seq") first then ", and"
                          else if isJust (secondContent >>= fValue "perfectBackground") then ", and"
                          else "and"
                        else if conj == "" then
                          if isJust (secondContent >>= fValue "accordingTo") then "; but"
                          else if isJust (secondContent >>= sValue "andEmphasis") then ""
                          else ","
                        else conj
        return $ m1 `cat` separator `cat` m2
  else f frame

np nom frame =
  if isSeq && (frame >>= fValue "member2" >>= getType) == Just "STREET" then
    handleSeq (return . streetName) frame `catM` return "streets"
  else if Just True == fmap (hasType "STREETS") frame then
    handleSeq (return . streetName) (frame >>= fValue "components") `catM` return "streets"
  else handleSeq (np_internal nom mayHaveDeterminer) frame where
  isSeq = (frame >>= getType) == Just "seq"
  isSubj = isJust (frame >>= usage "arg1")
  isAnchor = isJust (frame >>= usage "anchor")
  mayHaveDeterminer = if isNumber frame then isSubj || isAnchor || renderAsWord (fromJust frame) else True

np_internal :: Bool -> Bool -> Frame -> State GenerationState String
np_internal nom mayHaveDeterminer frame = do
  frameGenerated frame
  unquantified <- if hasType "ME" frame then if nom then return "I" else return "me"
    else if hasType "HE" frame then if nom then return "he" else return "him"
    else if hasType "SHE" frame then if nom then return "she" else return "her"
    else if hasType "THEY" frame then if nom then return "they" else return "them"
    else if hasType "WE" frame then if nom then return "we" else return "us"
    else if hasType "wh" frame then return $
      if isJust $ usage "arg1" frame >>= usage "content" >>= usage "relative" then "that"
      else if isAnimate frame then
        if Just "true" == sValue "negated" frame then "nobody" else if nom then "who" else "whom"
      else if isJust (usage "goal" frame) then "where"
      else "what"
    else do
      let n = if Just "true" == sValue "elided" frame then
                if Just "Pl" == sValue "rusNumber" frame then "ones" else "one"
              else noun (getType frame) frame
          adjs = foldl cat "" $ adjectives frame
          nbar1 = adjs `cat` n
      nbar <- case getType frame of
         Just "ORDER" | Just poss <- fValue "arg1" frame -> handleSeq (np_internal True False) (Just poss) `catM` return nbar1
         Just "STREET" | not (prefixName frame) -> return nbar1 `catM` return (streetName frame)
         _ | Just loc <- fValue "location" frame -> return nbar1 `catM` return "on" `catM` np False (Just loc)
         _ | Just src <- fValue "source" frame -> return nbar1 `catM` return "from" `catM` np False (Just src)
         _ -> return nbar1
      genitiveComplement <- case getType frame of
        Just "CORNER" -> case fValue "arg1" frame of
          Just gen -> return "of" `catM` np False (Just gen)
          _ -> return ""
        Just "OPINION" | not $ isDeterminerOpinion frame -> case fValue "arg1" frame of
          Just gen -> return "of" `catM` elideableArgument (Just gen) frame
          _ -> return ""
        _ -> return ""
      det <- if mayHaveDeterminer then determiner frame nbar else return ""
      return $ det `cat` nbar `cat` genitiveComplement
  let fQuantifier = fValue "quantifier" frame
  let postQuantifier = if (fQuantifier >>= getType) == Just "ALL" ||
                          (usage "arg1" frame >>= getType) == Just "DISPERSE"
                       then "all" else ""
  preQuantifier <- case fQuantifier >>= getType of
    Just "BOTH" -> return "both of"
    Just "ALL" -> return ""
    Just typ -> let q = handleSeq (np_internal True False) fQuantifier in
      if typ == "1" || isNothing (fValue "arg1" frame >>= getType) || any (\f -> fDeterminer frame == fDeterminer f) (prevSiblings frame) then q else q `catM` return "of"
    _ -> return ""
  relative <- fromMaybe (return "") $ liftM (catM $ return ", the one") $ fmap sentence $ fValue "relative" frame
  return $ preQuantifier `cat` unquantified `cat` postQuantifier `cat` relative

adjectives nounFrame = catMaybes [property, kind, shopKind, size, quality, gender] where
  property = fValue "property" nounFrame >>= getType >>= \p -> if p == "AMAZING" then Just "amazing" else Nothing
  kind = fValue "kind" nounFrame >>= getType >>= \p -> if p == "COMMERCIAL" then Just "commercial" else Nothing
  quality = fValue "quality" nounFrame >>= getType >>= \case
    "HUMBLE" -> Just "humble"
    "CLEVER" -> Just "smart"
    "STUPID" -> Just "stupid"
    _ -> Nothing
  shopKind = sValue "name" nounFrame >>= \p -> if p == "гастроном" then Just "grocery" else Nothing
  gender =
    if shouldContrastSubject nounFrame && isHuman nounFrame
    then case sValue "rusGender" nounFrame of
      Just "Masc" -> Just "male"
      Just "Fem" -> Just "female"
      _ -> Nothing
    else Nothing
  size = fValue "size" nounFrame >>= getType >>= \p ->
    if p == "LITTLE" then Just "small"
    else if p == "BIG" then
      if hasType "GARDEN" nounFrame then Just "big" else Just "great"
    else Nothing

shouldContrastSubject frame = let
  cp = usage "arg1" frame >>= usage "content"
  allCPs = flatten (fmap unSeq cp)
  allVerbs = catMaybes $ map (fValue "content") allCPs
  contrastibleSubject fVerb = case fValue "arg1" fVerb of
    Just fSubj | not (isNumber $ Just fSubj), Just g1 <- sValue "rusGender" fSubj, Just g2 <- sValue "rusGender" frame, g1 /= g2 ->
      not (isVerbEllipsis fVerb) || isEllipsisAnchor (Just fSubj) fVerb
    _ -> False
  in any contrastibleSubject allVerbs

streetName frame = case sValue "name" frame of
 Just "знаменская" -> "Znamenskaya"
 Just "бассейная" -> "Basseinaya"
 Just "театральная" -> "Teatralnaya"
 Just s -> s
 _ -> ""

fDeterminer frame =
  if hasAnyType ["NEIGHBORS", "AMAZE", "PREDICAMENT", "MOUTH", "NOSE", "JAW", "JAWS", "ARGUE", "FINGER", "SPEECH"] frame then fValue "arg1" frame
  else if hasAnyType ["OPINION"] frame && isDeterminerOpinion frame then fValue "arg1" frame
  else if hasAnyType ["WORDS"] frame then fValue "author" frame
  else if hasAnyType ["ROOMS", "APARTMENTS", "OFFICES"] frame then fValue "owner" frame
  else if hasAnyType ["CASHIER"] frame then fValue "place" frame
  else Nothing

isDeterminerOpinion frame = unSeq frame == frame && all (hasAnyType ["ME", "THEY", "HE", "SHE", "wh"]) (flatten $ fValue "arg1" frame)
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
            if Set.member det (visitedFrames state) then return $ case sValue "rusGender" det of
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
    Just _  | not $ any (\f -> fDeterminer frame == fDeterminer f) (prevSiblings frame) -> handleSeq genitiveSpecifier $ fmap resolve det
    _ -> return $
      let sDet = fValue "determiner" frame >>= getType in
      if sDet == Just "THIS" then "this"
      else if sDet == Just "ANY" then "any"
      else if isJust (fValue "quantifier" frame) then ""
      else if hasType "STREET" frame && prefixName frame then streetName frame
      else if hasAnyType ["SOME", "OTHERS", "THIS", "THAT", "JOY", "RELIEF", "MEANING", "MONEY", "COUNTING", "APARTMENTS", "OFFICES", "HOMES"] frame then ""
      else if hasAnyType ["NAMED_PERSON"] frame then ""
      else if hasType "OPINION" frame && Just True == fmap isVerbEllipsis (usage "accordingTo" frame) then ""
      else if sValue "given" frame == Just "true" then "the"
      else if "a" `isPrefixOf` nbar || "e" `isPrefixOf` nbar || "8" `isPrefixOf` nbar then "an"
      else if isSingular frame then "a"
      else ""

prefixName frame = earlier frame "name" frame "type"

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
    fromMaybe (return "???sentence") $ liftM clause $ fValue "content" frame
  finish = if sValue "dot" frame == Just "true" then "."
           else if sValue "question_mark" frame == Just "true" then "?"
           else case lastSentence >>= fValue "content" >>= fValue "message" of
             Just message -> if isNothing (getType message) then ":" else ""
             _ -> ""
  lastSentence = if hasType "seq" frame then fValue "member2" frame else Just frame

genComplement cp = case fValue "content" cp of
  Nothing -> return ""
  Just fVerb -> let
      prefix = if hasType "fact" cp && distinguish cp then "that" else ""
      negation = if Just "true" == sValue "negated" cp then "not" else ""
    in return (negation `cat` prefix) `catM` sentence cp

isGerund fVerb = hasAnyType ["SIT", "THINK"] fVerb &&
  (Just "FORGET" == (usage "content" (unSeq fVerb) >>= usage "arg2" . unSeq >>= getType) ||
  Just "ASK" == (usage "content" (unSeq fVerb) >>= usage "topic" . unSeq >>= getType))

verb verbForm frame = if isNothing (getType frame) then "???vp" else
  let negated = Just "true" == sValue "negated" frame && not (Just "true" == (fValue "arg1" frame >>= sValue "negated")) in
  case fromJust $ getType frame of
  "HAPPEN" -> "happened"
  "FORGET" -> "forgot"
  "DO" -> if verbForm == BaseVerb then "do" else "did"
  "GO" -> if verbForm == PastVerb then "went" else if verbForm == BaseVerb then "go" else "goes"
  "GO_OFF" -> "went"
  "ASK" -> if (fValue "topic" frame >>= getType) == Just "PREDICAMENT" then if verbForm == PastVerb then "consulted" else "consult" else if verbForm == BaseVerb then "ask" else "asked"
  "COME_SCALARLY" -> if sValue "time" frame == Just "PAST" then "went" else if verbForm == BaseVerb then "come" else "comes"
  "DISCOVER" -> "discovered"
  "DISTRACT" -> "distracted"
  "NEED" -> "need"
  "DISPERSE" -> "went"
  "SEE" -> "saw"
  "LOVE" -> if verbForm == BaseVerb then "love" else if negated then "doesn't love" else "loves"
  "THINK" -> if verbForm == BaseVerb then "think" else "thinking"
  "SIT" -> "sitting"
  "FALL" -> "fell"
  "BREAK" -> if verbForm == BaseVerb then "break" else "broke"
  "STOP" -> "stopped"
  "CAN" -> if negated then "couldn't" else "could"
  "BEGIN" -> "started"
  "COUNT" -> if verbForm == Gerund then "counting" else "count"
  "TO_WATER" -> if verbForm == Gerund then "watering" else "water"
  "DANCE" -> if verbForm == Gerund then "dancing" else "dance"
  "ARGUE" -> if verbForm == Gerund then "arguing" else if Just "true" == sValue "irrealis" frame then "were arguing" else "argue"
  "RECALL" -> "recall"
  "REMEMBER" -> if verbForm == PastVerb then "remembered" else if verbForm == Sg3Verb then "remembers" else "remember"
  "SMILE" -> "gave us a " ++ (if (fValue "manner" frame >>= getType) == Just "SADLY" then "sad " else "") ++ "smile"
  "THANK" -> "thanked"
  "RUN_OUT" -> "ran"
  "TAKE_OUT" -> "took"
  "GET_SAD" -> "got sad"
  "SAY" -> if isJust $ fValue "addressee" frame then "told" else if verbForm == PastVerb then "said" else "say"
  "MOVE" -> "moved"
  "SEEM" -> if isJust (usage "content" frame >>= usage "reason") then
     if verbForm == PastVerb then "were" else "is"
   else if verbForm == PastVerb then "seemed" else "seems"
  "copula" -> beForm (fValue "arg1" frame) (if sValue "time" frame /= Just "PAST" then BaseVerb else verbForm)
  typ -> typ

conjIntroduction fVerb =
   if sValue "butEmphasis" fVerb == Just "true" then "but"
   else if sValue "andEmphasis" fVerb == Just "true" then "and"
   else ""

distinguish frame = isNothing (usage "member2" frame) || Just "true" == sValue "distinguished" frame ||
  hasType "OPINION" frame && Just "WORDS" == (usage "member2" frame >>= fValue "member1" >>= getType)

elideableArgument frame parent = if any (\f -> source f == frame) (nextSiblings parent) then return "" else np False frame where
  source f = if hasType "WORDS" f then fValue "author" f else fValue "arg1" f

generateAccording parent = case fValue "accordingTo" parent of
  Just source -> do
    let isWh = isQuestioned source
        comma = if isEllipsisAnchor (fValue "arg1" parent) parent || isWh then "" else ","
        oneOpinion source = case getType source of
          Just "OPINION" -> return (if distinguish source then "in" else "") `catM` np False (Just source)
          Just "WORDS" -> return "according to" `catM` elideableArgument (fValue "author" source) source
          s -> return $ show s
    state <- get
    if Set.member source (visitedFrames state) then return ""
    else handleSeq oneOpinion (Just source) `catM` return comma
  _ -> return ""

clause :: Frame -> State GenerationState String
clause fVerb = do
    frameGenerated fVerb
    state <- get
    when (sValue "time" fVerb == Just "PAST") (put $ state { past = True })
    state <- get
    let intro = conjIntroduction fVerb
    let emphasis = if (fValue "optativeModality" fVerb >>= getType) == Just "LUCK" then "by some sheer luck,"
                  else if sValue "emphasis" fVerb == Just "true" then "there"
                   else if (fValue "relTime" fVerb >>= getType) == Just "AFTER" && isNothing (fValue "relTime" fVerb >>= fValue "anchor") then "then"
                   else if (fValue "relTime" fVerb >>= getType) == Just "BEFORE" && isNothing (fValue "relTime" fVerb >>= fValue "anchor") then "before,"
                   else ""
    let verbForm = if past state then PastVerb else if Just True == fmap (hasAnyType ["ME", "WE", "THEY"]) fSubject then BaseVerb else Sg3Verb
        isModality = hasType "modality" fVerb
        isRaising = hasType "SEEM" fVerb
        fSubject = if isModality || isRaising then fValue "theme" fVerb >>= fValue "arg1" else fValue "arg1" fVerb
        cp = usage "content" fVerb
    core <- if hasType "degree" fVerb && (fromMaybe False $ fmap (hasType "wh") $ fValue "arg2" fVerb)
           then return "Great was" `catM` np True fSubject
           else if hasType "copula" fVerb && isJust (fValue "owner" fVerb) then do
             let owner = fValue "owner" fVerb
             subj <- np True owner
             let verb = if verbForm == PastVerb then "had" else if Just "ME" == (owner >>= getType) then "have" else "has"
             obj <- np False (fValue "arg1" fVerb)
             return $ subj `cat` verb `cat` obj
           else vp fVerb verbForm FiniteClause
    elaboration <- case fValue "elaboration" fVerb of
      Just smth -> return (if hasType "HAPPEN" fVerb then "," else ":") `catM` sentence smth
      _ -> return ""
    let fComp = case getType fVerb of
          Just "FORGET" -> fValue "arg2" fVerb
          Just "ASK" -> fValue "topic" fVerb
          Just "DISCOVER" -> fValue "theme" fVerb
          Just "SAY" -> case fValue "message" fVerb of
            Just comp | isJust (getType comp) -> Just comp
            _ -> Nothing
          _ -> Nothing
    background <- case fValue "perfectBackground" fVerb of
      Just back -> case getType back of
        Just "MOVE" -> do
          let slightly = if Just "SLIGHTLY" == (fValue "manner" back >>= getType) then "slightly" else ""
          moved <- np False (fValue "arg2" back)
          return $ "moving" `cat` moved `cat` slightly `cat` "back and forth"
        Just "THINK" -> return "thinking carefully about" `catM` np False (fValue "theme" back)
        Just "COME_TO" ->
          let domain = case fValue "domain" back of
                         Just dom | isJust (sValue "type" dom) -> return "in" `catM` np False (Just dom)
                         _ -> return ""
          in return (conjIntroduction back `cat` "reaching") `catM` np False (fValue "goal_by" back) `catM` domain
        _ -> return ""
      _ -> return ""
    comp <- case fComp of
      Nothing -> return "" 
      Just cp -> let compVerb = fValue "content" cp in
        if hasType "question" cp && Just True == fmap (hasType "THINK") compVerb then
           do
             frameGenerated cp
             (return "about their opinion on") `catM` (np False $ fValue "topic" $ fromJust compVerb)
        else let comma = if not (hasType "SAY" fVerb) && hasType "fact" (head $ flatten fComp) then "," else ""
             in return comma `catM` handleSeq genComplement fComp
    externalComp <- if getType fVerb == Just "GO" then 
      case cp >>= usage "member1" >>= fValue "member2" of
       Just nextClause | (fValue "content" nextClause >>= getType) == Just "ASK" -> do
         frameGenerated nextClause
         return "to" `catM` vp (fromJust $ fValue "content" nextClause) BaseVerb InfiniteClause
       _ -> return ""
      else return ""
    condComp <- case fValue "whenCondition" fVerb of
      Just fComp -> return ", when" `catM` sentence fComp
      _ -> case fValue "ifCondition" fVerb of
        Just fComp -> return ", if" `catM` sentence fComp
        _ -> case fValue "condition" fVerb of
          Just caze | hasType "CASE" caze -> case msum [fValue "whenCondition" caze, fValue "ifCondition" caze] of
            Just fComp -> do
              comp <- sentence fComp
              according <- generateAccording caze
              return $ ", only if" `cat` (if null according then "" else ", " ++ according) `cat` comp
            _ -> return ""
          _ -> return ""
    reasonComp <- case fValue "reason" fVerb of
      Just fComp -> return "because" `catM` sentence fComp
      _ -> return ""
    questionVariants <- case cp >>= fValue "questioned" >>= fValue "variants" of
      Just variants -> (return "-") `catM` (np True (Just variants))
      _ -> return ""
    let coreWithBackground =
          if null background then core
          else if earlier fVerb "type" fVerb "perfectBackground" then core `cat` "," `cat` background `cat` ","
          else (if null emphasis then "" else ",") `cat` background `cat` "," `cat` core
    according <- generateAccording fVerb
    return $ intro `cat` emphasis `cat` according `cat` coreWithBackground `cat` condComp `cat` reasonComp `cat` comp `cat` externalComp `cat` questionVariants `cat` elaboration

isQuestioned frame = flip any (flatten $ Just frame) $ \frame ->
  hasType "wh" frame || Just True == fmap isQuestioned (fValue "arg1" frame) || Just True == fmap isQuestioned (fValue "author" frame)

beForm fSubject verbForm =
  if verbForm == PastVerb then
    if Just "Pl" == (fSubject >>= sValue "rusNumber") then "were" else "was"
  else if Just "ME" == (fSubject >>= getType) then "am" else "is"

data ClauseType = FiniteClause | InfiniteClause deriving (Eq)

vp :: Frame -> VerbForm -> ClauseType -> State GenerationState String
vp fVerb verbForm clauseType = do
  let preAdverb = case fValue "manner" fVerb >>= getType of
        Just "SUDDENLY" -> "suddenly"
        Just "JUST" -> "just"
        Just "SADLY" -> if getType fVerb == Just "SMILE" then "" else "sadly"
        Just "SLIGHTLY" -> if getType fVerb == Just "MOVE" then "" else "slightly"
        Just s -> s
        _ -> ""
      cp = usage "content" fVerb
      theme = fValue "theme" fVerb
      isFuture = Just "FUTURE" == sValue "time" fVerb
      isModality = hasType "modality" fVerb
      isRaising = hasType "SEEM" fVerb
      fSubject = if hasType "SEEM" fVerb || isModality || isRaising then theme >>= fValue "arg1" else fValue "arg1" fVerb
      isQuestion = Just True == fmap (hasType "question") cp
      nonSubjectQuestion = isQuestion && (cp >>= fValue "questioned") /= fSubject
      inverted = nonSubjectQuestion && Just "true" == (cp >>= sValue "question_mark")
      isDoModality = isModality && Just True == fmap (hasType "DO") theme
      thereSubject = clauseType == FiniteClause && (Just "wh" == (fSubject >>= getType) && isModality || isNothing (fSubject >>= getType)) && not isQuestion
      sVerb = if isEllipsisAnchor fSubject fVerb
              then
                if fSubject == (cp >>= fValue "ellipsisAnchor2") then if verbForm == PastVerb then "did" else "does"
                else "-"
              else if isModality then
                if isQuestion then if isJust fSubject && isDoModality then "supposed" else ""
                else if thereSubject then if verbForm == PastVerb then "was" else "is"
                else if verbForm == PastVerb then "had" else if isFuture then "will have" else "have"
              else verb (if null aux then verbForm else if isGerund fVerb then Gerund else BaseVerb) fVerb
      finalAdverb = case getType fVerb of
        Just "HAPPEN" -> "today"
        Just "MOVE" -> (if Just "SLIGHTLY" == (fValue "manner" fVerb >>= getType) then "slightly" else "") `cat` "back and forth"
        _ -> ""
      negation = if sValue "negated" fVerb == Just "true" && isGerund fVerb then "not" else ""
      aux =
        if isGerund fVerb && isNothing (usage "content" fVerb >>= usage "member2") then beForm fSubject verbForm
        else if isModality && isQuestion then
          if isNothing fSubject then ""
          else if isDoModality then beForm fSubject verbForm
          else "should"
        else if inverted then if verbForm == PastVerb then "did" else if Just True == fmap (hasAnyType ["ME", "THEY"]) fSubject then "do" else "does"
        else ""
      allArgs = arguments fVerb
      topicalizedArg = case (fSubject, allArgs) of
        (Just subj, hd@(PPAdjunct _ value):_) | earlier value "type" fVerb "type" && earlier value "type" subj "type" -> Just hd
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
        Just (PPArg prep val) -> if isJust (usage "goal" val) then "" else prep
        _ -> ""
      anymore = let
        isAnymore = Just "true" == (existentialWhArg >>= argumentFrame >>= sValue "not_anymore") ||
                    clauseType == FiniteClause && Just "wh" == (fSubject >>= getType) && Just "true" == (fSubject >>= sValue "not_anymore") ||
                    Just "true" == sValue "not_anymore" fVerb
        in if isAnymore then "anymore" else ""
  subject <- if thereSubject then return "there" else case (fSubject, clauseType) of
    (Just f, FiniteClause) ->
      if thereSubject then return "there"
      else if isVerbEllipsis fVerb && not (isEllipsisAnchor fSubject fVerb) then return "it"
      else if [fVerb] `isPrefixOf` (usages "arg1" f) || isModality || isRaising then np True fSubject
      else if (isJust $ fValue "perfectBackground" fVerb) then return $ if sValue "rusGender" f == Just "Masc" then "he" else "she"
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
      if Just "true" == sValue "negated" frame then
        if Just "true" == sValue "animate" frame then "nobody" else if isJust $ usage "goal" frame then "nowhere" else "nothing"
      else if Just "true" == sValue "animate" frame then "somebody" else "something"
    _ -> return ""
  according <- if null whWord && Just True /= fmap (hasType "wh") fSubject then return "" else do
    acc <- generateAccording fVerb
    return $ if null acc then "" else "," `cat` acc
  controlled <- case getType fVerb of
    Just "CAN" -> case theme of
      Just slave -> vp slave BaseVerb InfiniteClause
      _ -> return ""
    Just "modality" -> case theme of
       Just slave -> return (if isDoModality || not isQuestion || isNothing fSubject then "to" else "") `catM` vp slave BaseVerb InfiniteClause
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