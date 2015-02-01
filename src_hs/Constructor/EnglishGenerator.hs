module Constructor.EnglishGenerator (generate) where
import Constructor.Sense
import Constructor.Inference
import Constructor.ArgumentPlanning
import Constructor.EnglishNouns
import Constructor.EnglishVerbs
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
              separator = if null output || "\n" `isSuffixOf` output then "" else if start == "- " then "\n" else " "
          return $ output ++ separator ++ start ++ capitalize (stripFirstComma nextSentence)
      text = evalState sentenceState $ GenerationState Set.empty False
  in stripLastComma text

capitalize (c:rest) = (toUpper c):rest

getTopFrame frame = if isCP frame && isNothing (usage P.Relative frame) then upmostSeq frame else Nothing where
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
      let skipSecond frame = let
            externalFrame = fValue P.Member1 frame >>= fValue P.Content >>= getExternalComp >>= argumentFrame
            secondContent = fValue P.Member2 frame >>= fValue P.Content
            in isJust externalFrame && (externalFrame == secondContent || externalFrame == (secondContent >>= fValue P.Theme))
          lastGeneratedMember seqFrame =
            if not (hasType "seq" seqFrame) || not (skipSecond seqFrame) then Just seqFrame
            else fValue P.Member1 seqFrame >>= lastGeneratedMember
      if skipSecond frame then return m1 else do
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
                          else if length (filter (\cp -> Just True == fmap (hasType "copula") (fValue P.Content cp)) $ flatten $ Just frame) > 1 then ", and"
                          else if Just "true" == (second >>= sValue P.Negated) then "and"
                          else ", but"
                        else if conj == "and" then
                          if Just True == fmap isCP second && Just True == fmap (hasType "seq") (first >>= lastGeneratedMember) then ", and"
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
  state <- get
  if Set.member frame (visitedFrames state) then return $ fromMaybe "ONE" $ getType frame else do
  frameGenerated frame
  let asPronoun = case getType frame of
        Just "ME" -> if nom then "I" else "me"
        Just "WE" -> if nom then "we" else "us"
        Just "THEY" -> if nom then "they" else "them"
        Just s ->
          if isHuman $ resolve frame then
            if s == "HE" then if nom then "he" else "him"
            else if s == "SHE" then if nom then "she" else "her"
            else s
          else "it"
        _ -> "???"

  unquantified <- if hasAnyType ["ME", "WE", "THEY", "HE", "SHE"] frame then return asPronoun
    else if hasType "EVERYTHING" frame then return "everything"
    else if hasType "EVERYBODY" frame then return "everybody"
    else if hasType "wh" frame then return $
      if isJust (usage P.Goal frame) then "where"
      else if isJust (usage P.VTime frame) then "when"
      else if isJust (usage P.Location frame) then "where"
      else if isAnimate frame then
        if Just "true" == sValue P.Negated frame then "nobody"
        else if Just "true" == (usage P.Arg2 frame >>= sValue P.ProfessionCopula) then "what"
        else if nom then "who" else "whom"
      else if isJust $ usage P.Questioned frame >>= usage P.Relative then "that"
      else "what"
    else do
      let n = if Just "true" == sValue P.Elided frame then
                if Just "Pl" == sValue P.RusNumber frame then "ones" else "one"
              else noun (getType frame) frame
          adjs = foldl cat "" $ adjectives frame
          nbar1 = adjs `cat` n
          fDet = fDeterminer frame
      nbar <- case getType frame of
         Just "ORDER" | Just poss <- fValue P.Arg1 frame -> handleSeq (np_internal True False) (Just poss) `catM` return nbar1
         Just "STREET" | not (prefixName frame) -> return nbar1 `catM` return (streetName frame)
         _ | Just loc <- fValue P.Location_on frame -> return nbar1 `catM` return "on" `catM` np False (Just loc)
         _ | Just src <- fValue P.Source frame -> return nbar1 `catM` return "from" `catM` np False (Just src)
         _ -> return nbar1
      spec <- if mayHaveDeterminer then determiner frame fDet nbar else return ""
      genitiveComplement <- case fDet of
        Just det | not mayHaveDeterminer || shouldGenerateDeterminer frame det state False ->
          return "of" `catM` elideableArgument fDet frame
        _ -> return ""
      return $ spec `cat` nbar `cat` genitiveComplement
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
    _ -> return $
      if Just "SUCH" == (fValue P.Determiner frame >>= getType) then
        if Just True == fmap isExclamationCopula (usage P.Arg2 frame) then "what" else "such"
      else if null postQuantifier then allOf
      else ""
  relative <- case fValue P.Relative frame of
    Just relativeCp -> let rel = sentence relativeCp in
      if Just "copula" == (fValue P.Content relativeCp >>= getType) then return ", the one" `catM` rel
      else rel
    _ -> return ""
  let neg = if Just "true" == sValue P.Negated frame && not (hasType "wh" frame) then "not" else ""
  return $ neg `cat` preQuantifier `cat` unquantified `cat` postQuantifier `cat` relative

adjectives nounFrame = catMaybes [property, kind, shopKind, size, quality, gender, color] where
  property = fValue P.Property nounFrame >>= getType >>= \p -> if p == "AMAZING" then Just "amazing" else Nothing
  kind = fValue P.Kind nounFrame >>= getType >>= \p -> if p == "COMMERCIAL" then Just "commercial" else Nothing
  quality = fValue P.Quality nounFrame >>= getType >>= \case
    "HUMBLE" -> Just "humble"
    "CLEVER" -> Just "smart"
    "STUPID" -> Just "stupid"
    _ -> Nothing
  color = fValue P.Color nounFrame >>= getType >>= \case
    "GREEN" -> Just "green"
    "RED" -> Just "red"
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
    else if p == "EXCESSIVE" then Just "excessive"
    else if p == "BIG" then
      if hasType "GARDEN" nounFrame then Just "big" else Just "great"
    else Nothing

shouldContrastSubject frame = let
  allVerbs = fromMaybe [] $ fmap allCoordinatedVerbs $ usage P.Arg1 frame
  contrastibleSubject fVerb = case fValue P.Arg1 fVerb of
    Just fSubject | not (isNumber $ Just fSubject), Just g1 <- sValue P.RusGender fSubject, Just g2 <- sValue P.RusGender frame, g1 /= g2 ->
      not (isVerbEllipsis fVerb) || isEllipsisAnchor (Just fSubject) fVerb
    _ -> False
  in any contrastibleSubject allVerbs

streetName frame = case fValue P.VName frame >>= sValue P.Name of
 Just "знаменская" -> "Znamenskaya"
 Just "бассейная" -> "Basseinaya"
 Just "театральная" -> "Teatralnaya"
 Just s -> s
 _ -> ""

fDeterminer frame =
  if hasAnyType ["NEIGHBORS", "AMAZE", "PREDICAMENT", "MOUTH", "NOSE", "JAW", "JAWS", "ARGUE", "FINGER", "SPEECH", "FAMILY", "EYES", "BROTHER", "SISTER", "CORNER", "CURIOSITY"] frame then fValue P.Arg1 frame
  else if hasAnyType ["OPINION"] frame then fValue P.Arg1 frame
  else if hasAnyType ["WORDS", "BOOK"] frame then fValue P.Author frame
  else if hasAnyType ["ROOMS", "APARTMENTS", "OFFICES"] frame then fValue P.Owner frame
  else if hasAnyType ["CASHIER"] frame then fValue P.Place frame
  else Nothing

usePronoun state frame = Set.member frame (visitedFrames state)
isPronoun = hasAnyType ["ME", "THEY", "HE", "SHE", "wh"]

isHeavyNP :: GenerationState -> Maybe Frame -> Bool
isHeavyNP state mNoun = Just True == fmap isHeavyNoun mNoun where
  isHeavyNoun noun =
    if usePronoun state noun then False
    else if hasType "seq" noun then any (not . isPronoun) (flatten $ Just noun)
    else isJust (fValue P.Relative noun) || isJust (fValue P.Components noun) || isHeavyNP state (fDeterminer noun)

shouldGenerateDeterminer noun det state asSpecifier = let
  prev = filter (\f -> fDeterminer f == Just det) $ prevSiblings noun
  next = filter (\f -> fDeterminer f == Just det) $ nextSiblings noun
  in
  if not (null prev) && Set.member det (visitedFrames state) then False
  else if hasType "OPINION" noun && not (isPronoun det) then not asSpecifier
  else if isHeavyNP state $ Just det then not asSpecifier
  else if any (hasType "WORDS") $ prev ++ next then not asSpecifier
  else if null prev then asSpecifier
  else if null next then not asSpecifier
  else False

determiner frame det nbar = do
  state <- get
  let genitiveSpecifier det =
        let pronoun s = do frameGenerated det; return s in
        case getType $ resolve det of
          Just "ME" -> pronoun "my"
          Just "HE" -> pronoun "his"
          Just "THEY" -> pronoun "their"
          Just "WE" -> pronoun "our"
          Just "SHE" -> pronoun "her"
          Just "wh" -> pronoun "whose"
          Just s ->
            if usePronoun state det then pronoun $ case sValue P.RusGender det of
               Just "Masc" -> "his"
               Just "Fem" -> "her"
               Just "Neu" -> "its"
               _ -> s
            else do
              let human = isHuman det
              let allowInnerDeterminer = not $ hasAnyType ["OPINION", "WORDS"] frame
              sDet <- np_internal False allowInnerDeterminer det
              return $ if human then if "s" `isSuffixOf` sDet then sDet ++ "'" else sDet ++ "'s" else sDet
          _ -> return ""
  case det of
    Just _det | shouldGenerateDeterminer frame _det state True ->
      let own = if hasType "SELF" _det && hasType "EYES" frame && isNothing (fValue P.Quantifier frame) then "own" else ""
      in handleSeq genitiveSpecifier det `catM` return own
    _ -> return $
      let sDet = fValue P.Determiner frame >>= getType in
      if sDet == Just "THIS" then "this"
      else if sDet == Just "THAT" then "that"
      else if sDet == Just "ANY" then "any"
      else if sDet == Just "ANOTHER" then "another"
      else if sDet == Just "wh" then "which"
      else if isJust (fValue P.Quantifier frame) then ""
      else if hasType "STREET" frame && prefixName frame then streetName frame
      else if hasAnyType ["SOME", "OTHERS", "THIS", "THAT", "JOY", "RELIEF", "MEANING", "MONEY", "COUNTING", "APARTMENTS", "OFFICES", "HOUSES"] frame then ""
      else if hasAnyType ["NAMED_PERSON"] frame then ""
      else if hasType "OPINION" frame && Just True == fmap isVerbEllipsis (usage P.AccordingTo frame) then ""
      else if sValue P.Given frame == Just "true" then "the"
      else if any (\c -> [c] `isPrefixOf` nbar) "aeiou8" then "an"
      else if isSingular frame then "a"
      else ""

prefixName frame = case fValue P.VName frame of
  Just fName -> earlier fName P.Name frame P.Type
  _ -> False

cat "" t2 = t2
cat t1 "" = t1
cat t1 t2 = case t2 of
 [] -> t1
 c:_ -> if c `elem` ",.:;?!\n" then stripLastComma t1 ++ t2 else t1 ++ " " ++ t2

stripLastComma t1 = if "," `isSuffixOf` t1 then take (length t1 - 1) t1 else t1
stripFirstComma t1 = if ", " `isPrefixOf` t1 then drop 2 t1 else t1

catM :: State GenerationState String -> State GenerationState String -> State GenerationState String
catM t1 t2 = do s1 <- t1; s2 <- t2; return $ s1 `cat` s2

frameGenerated frame = do state <- get; put $ state { visitedFrames = Set.insert frame $ visitedFrames state }

sentence :: Frame -> State GenerationState String
sentence frame = handleSeq singleSentence (Just frame) `catM` return (finish ++ newline) where
  singleSentence frame = do
    frameGenerated frame
    let content = fValue P.Content frame
    if Just "object" == sValue P.SituationKind frame
    then np True content
    else fromMaybe (return " ???sentence") $ liftM clause content
  finish = if sValue P.Dot frame == Just "true" then "."
           else if sValue P.Question_mark frame == Just "true" then "?"
           else if sValue P.Exclamation_mark frame == Just "true" then "!"
           else case lastSentence >>= fValue P.Content >>= fValue P.Message of
             Just message -> if isNothing (getType message) then ":" else ""
             _ -> ""
  newline = if sValue P.ParagraphEnd frame == Just "true" then "\n"
            else if sValue P.SectionEnd frame == Just "true" then "\n\n"
            else ""
  lastSentence = if hasType "seq" frame then fValue P.Member2 frame else Just frame

genComplement cp = case fValue P.Content cp of
  Nothing -> return ""
  Just fVerb -> let
      prefix = if isFactCP cp && distinguish cp then "that" else ""
      negation = if Just "true" == sValue P.Negated cp then "not" else ""
    in return (negation `cat` prefix) `catM` sentence cp

conjIntroduction fVerb =
   if sValue P.ButEmphasis fVerb == Just "true" then "but"
   else if sValue P.AndEmphasis fVerb == Just "true" then "and"
   else ""

distinguish frame = isNothing (usage P.Member2 frame) || Just "true" == sValue P.Distinguished frame ||
  hasType "OPINION" frame && Just "WORDS" == (usage P.Member2 frame >>= fValue P.Member1 >>= getType)

elideableArgument frame parent = if any (\f -> fDeterminer f == frame) (nextSiblings parent) then return "" else np False frame

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

getExternalComp fVerb = usage P.Content fVerb >>= usage P.Member1 >>= fValue P.Member2 >>= fValue P.Content >>= \nextVerb ->
  if fValue P.Arg1 fVerb /= fValue P.Arg1 nextVerb then Nothing
  else if hasType "GO" fVerb && hasType "ASK" nextVerb then Just $ ToInfinitive nextVerb
  else if hasType "LEAN_OUT" fVerb && hasType "BEGIN" nextVerb then case fValue P.Theme nextVerb of
    Just theme | hasType "LOOK" theme -> Just $ GerundBackground AfterVerb theme
    _ -> Nothing
  else if hasType "FALL_OUT" fVerb && hasType "FALL" nextVerb then Just $ Silence nextVerb
  else Nothing

clause :: Frame -> State GenerationState String
clause fVerb = do
    frameGenerated fVerb
    state <- get
    when (sValue P.Time fVerb == Just "PAST") (put $ state { past = True })
    when (sValue P.Time fVerb == Just "FUTURE") (put $ state { past = False })
    state <- get
    let intro = conjIntroduction fVerb
    let emphasis = if (fValue P.OptativeModality fVerb >>= getType) == Just "LUCK" then "by some sheer luck,"
                   else if (fValue P.OptativeModality fVerb >>= getType) == Just "BY_THE_WAY" then "by the way,"
                   else if sValue P.Emphasis fVerb == Just "true" then "there"
                   else if (fValue P.RelTime fVerb >>= getType) == Just "AFTER" && isNothing (fValue P.RelTime fVerb >>= fValue P.Anchor) then "then"
                   else if (fValue P.RelTime fVerb >>= getType) == Just "BEFORE" && isNothing (fValue P.RelTime fVerb >>= fValue P.Anchor) then "before,"
                   else ""
    let fSubject = englishSubject fVerb
        cp = usage P.Content fVerb
    core <- if hasType "degree" fVerb && Just True == fmap (hasType "wh") (fValue P.Arg2 fVerb)
           then case fSubject >>= getType of
             Just "AMAZE" -> return "Great was" `catM` np True fSubject
             Just "CUNNING_PERSON" -> return "What" `catM` np True fSubject
             _ -> return "??degree"
           else vp fVerb (determineVerbForm fSubject $ past state) FiniteClause
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
    comp <- case fComp of
      Nothing -> return "" 
      Just cp -> let compVerb = fValue P.Content cp in
        if not $ isCPOrSeq cp then return ""
        else if isQuestionCP cp && Just True == fmap (hasType "THINK") compVerb then
           do
             frameGenerated cp
             (return "about their opinion on") `catM` (np False $ fValue P.Topic $ fromJust compVerb)
        else let comma = if not (hasType "SAY" fVerb) && isFactCP (head $ flatten fComp) then "," else ""
             in return comma `catM` handleSeq genComplement fComp
    externalComp <- fromMaybe (return "") $ fmap generateArg $ getExternalComp fVerb
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
      Just fComp | hasType "situation" fComp -> return "because" `catM` sentence fComp
      _ -> return ""
    questionVariants <- case cp >>= fValue P.Questioned >>= fValue P.Variants of
      Just variants -> return "-" `catM` np ((cp >>= fValue P.Questioned) == fSubject) (Just variants)
      _ -> return ""
    according <- generateAccording fVerb
    return $ cat intro $ stripFirstComma $ emphasis `cat` according `cat` core `cat` controlledComp `cat` condComp `cat` reasonComp `cat` comp `cat` externalComp `cat` questionVariants `cat` elaboration

isQuestioned frame = flip any (flatten $ Just frame) $ \frame ->
  hasType "wh" frame ||
  Just True == fmap isQuestioned (fValue P.Arg1 frame) ||
  Just True == fmap isQuestioned (fValue P.Author frame) ||
  Just True == fmap isQuestioned (fValue P.Determiner frame)

data ClauseType = FiniteClause | InfiniteClause deriving (Eq)

vp :: Frame -> VerbForm -> ClauseType -> State GenerationState String
vp fVerb verbForm clauseType = do
  let cp = usage P.Content fVerb
      theme = fValue P.Theme fVerb
      isModality = hasType "modality" fVerb
      isRaising = hasType "SEEM" fVerb
      fSubject = englishSubject fVerb
      isQuestion = Just True == fmap isQuestionCP cp
      nonSubjectQuestion = isQuestion && (isNothing fSubject || not (fromJust fSubject `elem` flatten (cp >>= fValue P.Questioned)))
      inverted = nonSubjectQuestion && Just "true" == (cp >>= sValue P.Question_mark)
      isDoModality = isModality && Just True == fmap (hasType "DO") theme
      thereSubject = clauseType == FiniteClause && (Just "wh" == (fSubject >>= getType) && isModality || isNothing (fSubject >>= getType)) && not isQuestion
      (_aux, sVerb) = generateVerbs fVerb fSubject verbForm inverted isModality isQuestion isDoModality thereSubject
      aux = if clauseType == InfiniteClause then "" else _aux
      finalAdverb = case getType fVerb of
        Just "HAPPEN" -> "today"
        Just "copula" | isExclamationCopula fVerb && isAtLocationCopula fVerb -> "here"
        Just "MOVE" -> (if Just "SLIGHTLY" == (fValue P.Manner fVerb >>= getType) then "slightly" else "") `cat` "back and forth"
        _ -> ""
      negation = if sValue P.Negated fVerb == Just "true" && isGerund fVerb then "not" else ""
      allArgs = if isModality then fromMaybe [] (fmap arguments theme) else arguments fVerb
      prefixArgs = filter (\a -> argPosition a == BeforeVP) allArgs
      infixArgs = filter (\a -> argPosition a == BeforeVerb) allArgs
      postfixArgs = filter (\a -> argPosition a == AfterVerb) allArgs
      topicalizedArg = case (fSubject, postfixArgs) of
        (Just subj, hd@(PPAdjunct _ _ value):_) | typeEarlier value fVerb && typeEarlier value subj -> Just hd
        _ -> if isExclamationCopula fVerb then listToMaybe [a | a@(NPArg v) <- postfixArgs, Just v == fValue P.Arg2 fVerb] else Nothing
      questionedArg = if not nonSubjectQuestion then Nothing else Data.List.find isQuestionedArg postfixArgs
      existentialWhArg =
        if isQuestion then Nothing
        else if clauseType == FiniteClause && Just True == fmap isQuestioned fSubject && isModality then Just (NPArg $ fromJust fSubject)
        else Data.List.find isQuestionedArg postfixArgs
      isQuestionedArg arg = Just True == fmap isQuestioned (argumentFrame arg)
      removeMaybe maybeVal list = fromMaybe list $ fmap (flip Data.List.delete list) maybeVal
      normalArgs = removeMaybe questionedArg $ removeMaybe topicalizedArg $ removeMaybe existentialWhArg $ postfixArgs
      stranded = case mplus questionedArg existentialWhArg of
        Just (PPArg prep val) -> if isJust (usage P.Goal val) then "" else prep
        _ -> ""
      anymore = let
        isAnymore = Just "true" == (existentialWhArg >>= argumentFrame >>= sValue P.Not_anymore) ||
                    clauseType == FiniteClause && Just "wh" == (fSubject >>= getType) && Just "true" == (fSubject >>= sValue P.Not_anymore) ||
                    Just "true" == sValue P.Not_anymore fVerb
        in if isAnymore then "anymore" else ""
  subject <- if thereSubject then return "there" else if Just "WEATHER_BE" == getType fVerb then return "it" else case (fSubject, clauseType) of
    (Just f, FiniteClause) ->
      if isVerbEllipsis fVerb && not (isEllipsisAnchor fSubject fVerb) then return "it"
      else if [fVerb] `isPrefixOf` (usages P.Arg1 f) || isModality || isRaising || isAtLocationCopula fVerb || isOwnerCopula fVerb then np True fSubject
      else if (isJust (msum [fValue P.PerfectBackground fVerb, fValue P.Reason fVerb]) || hasType "copula" fVerb)
        then return $ if sValue P.RusGender f == Just "Masc" then "he" else "she"
      else return ""
    _ -> return ""
  beforeVP <- foldM (\s arg -> return s `catM` generateArg arg) "" prefixArgs
  preAdverb <- foldM (\s arg -> return s `catM` generateArg arg) "" infixArgs
  sArgs <- foldM (\s arg -> return s `catM` generateArg arg) "" $ Data.List.sortBy (compare `on` argOrder) normalArgs
  sTopicalized <- case topicalizedArg of
    Just arg -> generateArg arg `catM` return (if isExclamationCopula fVerb then "" else ",")
    _ -> return ""
  whWord <- case questionedArg >>= argumentFrame of
    Just qFrame -> np (hasType "wh" qFrame) (Just qFrame)
    _ -> return ""
  nonWhWord <- case existentialWhArg >>= argumentFrame of
    Just frame -> return $
      if Just "true" == sValue P.Negated frame then
        if Just "true" == sValue P.Animate frame then "nobody"
        else if isJust $ usage P.Goal frame then "nowhere"
        else if Just "true" == (fSubject >>= sValue P.Negated) then "anything"
        else "nothing"
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
  let (mainVerb, restVerb) = if null aux then (sVerb, "") else (aux, sVerb)
  let shortForm = case mainVerb of
        "am" -> "'m"
        "is" -> "'s"
        "will" -> "'ll"
        _ -> ""
  let contractableSubject = subject `elem` ["I", "he", "she", "we", "it", "what", "that", "it", "there"]
  let contracted = if null preAdverb && null negation && null according && not inverted && contractableSubject then
                     if null shortForm then subject `cat` mainVerb `cat` restVerb
                     else (subject ++ shortForm) `cat` restVerb
                   else (if inverted then according `cat` aux `cat` negation `cat` subject else subject `cat` according `cat` aux `cat` negation) `cat` preAdverb `cat` sVerb
  return $ beforeVP `cat` sTopicalized `cat` whWord `cat` contracted `cat` nonWhWord `cat` controlled `cat` sArgs `cat` stranded `cat` anymore `cat` finalAdverb

generateArg :: Argument -> State GenerationState String
generateArg arg = let
  hybridWhPrefix frame =
    if hasType "wh" frame && Just "true" == (usage P.Member2 frame >>= sValue P.Hybrid) then
      if Just "and" == sValue P.Conj (unSeq frame) then ", and" else ","
    else ""
  in case arg of
    Adverb _ s -> return s
    NPArg f -> np False $ Just f
    PPArg prep f ->
      if isJust (getType f) then return (hybridWhPrefix f) `catM` return prep `catM` (np False $ Just f) else return ""
    PPAdjunct _ prep f -> return prep `catM` (np False $ Just f)
    ToInfinitive nextVerb -> return "to" `catM` vp nextVerb BaseVerb InfiniteClause
    GerundBackground _ back -> return ("," `cat` conjIntroduction back) `catM` vp back Gerund InfiniteClause `catM` return ","
    CommaSurrounded a -> return "," `catM` generateArg a `catM` return ","
    Silence _ -> return ""

argOrder arg = case arg of
  PPAdjunct {} -> 2
  Adverb _ "already" -> 0
  NPArg {} -> 0
  CommaSurrounded a -> argOrder a
  _ -> 1