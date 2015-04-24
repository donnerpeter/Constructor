module Constructor.EnglishGenerator (generate) where
import Constructor.Sense
import Constructor.Inference
import Constructor.ArgumentPlanning
import Constructor.EnglishNouns
import Constructor.EnglishPronouns
import Constructor.EnglishVerbs
import Control.Monad.State
import Control.Applicative
import Data.List
import Data.Char (toUpper)
import Data.Maybe
import qualified Data.Set as Set
import Constructor.Util
import Data.Function (on)
import qualified Constructor.SemanticProperties as P

data GenerationState = GenerationState { visitedFrames:: Set.Set Frame, visitedSentenceFrames:: Set.Set Frame, past:: Bool }

generate:: Sense -> String
generate sense =
  let topFrames = catMaybes $ map getTopFrame $ allFrames sense
      sentenceState = foldM generateSentence [] topFrames
      generateSentence :: String -> Frame -> State GenerationState String
      generateSentence output frame = do
        state <- get
        if Set.member frame (visitedFrames state) then return output
        else do
          put $ state { visitedSentenceFrames = Set.empty }
          nextSentence <- sentence frame
          let start = if (sValue P.DirectSpeech frame) == Just "true" then "- " else ""
              separator = if null output || "\n" `isSuffixOf` output then "" else if start == "- " then "\n" else " "
          return $ output ++ separator ++ start ++ capitalize (stripFirstComma nextSentence)
      text = evalState sentenceState $ GenerationState Set.empty Set.empty False
  in stripLastComma text

capitalize (c:rest) = (toUpper c):rest
capitalize [] = []

getTopFrame frame = if isCP frame && null (allUsages [P.Relative, P.WhenCondition, P.IfCondition] frame) then upmostSeq frame else Nothing where
  upmostSeq frame =
    case usage P.Member1 frame of
      Just p | Just ";" /= sValue P.Conj p -> upmostSeq p
      _ | Just p <- usage P.Member2 frame -> if Just ";" == sValue P.Conj p then Just frame else Nothing
      _ -> Just frame

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
            contents = catMaybes $ map (fValue P.Content) $ flatten $ Just frame
            copulas = filter (hasType "copula") contents
            copulaValues = catMaybes $ map (fValue P.Arg2) copulas
            separator = if conj == "but" then
                          if Just "true" == (firstContent >>= sValue P.Irrealis) then ", when"
                          else if (Just True == fmap isGerund secondContent || Just True == fmap isGerund firstContent) &&
                                  isNothing (firstContent >>= sValue P.Negated)
                            then "and"
                          else if length copulaValues > 1 then
                            if any (hasType "MORE") $ concatMap flatten $ map (fValue P.Quality) copulaValues then ", but"
                            else ", and"
                          else if Just True == fmap shouldContrastByGender (firstContent >>= fValue P.Arg1) then ", and"
                          else if Just "true" == (second >>= sValue P.Negated) && Just "true" /= sValue P.ConjStrong frame then "and"
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
        return $ m1 `cat` separator `cat` (stripFirstComma m2)
  else f frame

np nom frame =
  if isSeq && (frame >>= fValue P.Member2 >>= getType) == Just "STREET" then
    handleSeq (return . streetName) frame `catM` return "streets"
  else if Just True == fmap (hasType "STREETS") frame then
    handleSeq (return . streetNameString) (frame >>= fValue P.VName) `catM` return "streets"
  else handleSeq (np_internal nom mayHaveDeterminer) frame where
  isSeq = (frame >>= getType) == Just "seq"
  isSubj = isJust (frame >>= usage P.Arg1)
  isAnchor = isJust (frame >>= usage P.Anchor)
  mayHaveDeterminer = if isNumber frame then isSubj || isAnchor || renderAsWord (fromJust frame) else True

np_internal :: Bool -> Bool -> Frame -> State GenerationState String
np_internal nom mayHaveDeterminer frame = do
  state <- get
  if Set.member frame (visitedSentenceFrames state) then return $ fromMaybe "ONE" $ getType frame else do
  frameGenerated frame

  unquantified <-
    if hasType "wh" frame then return $ whWord nom frame
    else if isPronoun frame then return $ npPronoun nom frame
    else if hasType "EVERYTHING" frame then return "everything"
    else if hasType "EVERYBODY" frame then return "everybody"
    else do
      adjs <- adjectives frame
      let n = if isElidedNoun frame then
                if skipElidedOne frame || Just True == fmap isPronoun fDet || Just "BLIND" == (getType =<< fValue P.Quality frame)|| Just "UNEMBRACEABLE" == (getType =<< fValue P.Size frame) then ""
                else if Just "Pl" == sValue P.RusNumber frame then "ones" else "one"
              else if isElided frame || isPlaceholder frame then ""
              else noun (getType frame) frame
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
    _ ->
      if isElidedNoun frame && Just "SMASHED" == (getType =<< fValue P.Quality frame) then let
        verb = if past state then "was" else "is"
        in return $ "who" `cat` verb `cat` "smashed"
      else return ""
  let neg = if Just "true" == sValue P.Negated frame && not (hasType "wh" frame) then "not" else ""
  let emphasis = if Just "true" == sValue P.AndEmphasis frame then "even" else ""
  return $ neg `cat` emphasis `cat` preQuantifier `cat` unquantified `cat` postQuantifier `cat` relative

isArticleAfterAdjectives mainFrame =
  not (isPlaceholder mainFrame) &&
  length adjFrames > 1 && sValue P.Negated (head adjFrames) == Just "true"
  where
  adjFrames = flatten (fValue P.Color mainFrame)

isPlaceholder frame = hasType "placeholder" frame

isElided frame = Just "true" == sValue P.Elided frame
isElidedNoun frame = Just "true" == sValue P.ElidedNoun frame

skipElidedOne nounFrame = Just "true" == (sValue P.ConjStrong $ unSeq2 $ unSeq1 nounFrame) && case prevSiblings $ unSeq1 nounFrame of
  first:_ | not (isElidedNoun first) -> True
  _ -> False

adjectives nounFrame = do
  let adjSeq attr fun = let
        value = fValue attr nounFrame
        negation frame = if Just "true" == sValue P.Negated frame then "not" else ""
        modifiers frame = if Just "JUST" == sValue P.ModifierAdverb frame then "just"
                          else if Just "ONLY" == sValue P.ModifierAdverb frame then "only"
                          else ""
        adjOrMore frame = do
          let emph = if Just "true" == sValue P.Emphasis frame then "even" else ""
          anchor <- case fValue P.Anchor frame of
            Just a -> return "than" `catM` np False (Just a)
            _ -> return ""
          let comparative = case getType =<< fValue P.Theme frame of
                Just "CLEVER" -> "smarter"
                Just "FAST" -> "faster"
                Just "BIG" -> "larger"
                Just "GOOD" -> "better"
                _ -> "more"
          if hasType "MORE" frame then return $ negation frame `cat` emph `cat` modifiers frame `cat` comparative `cat` anchor else eachAdj frame
        eachAdj adjFrame = let
          article = if isArticleAfterAdjectives nounFrame then indefiniteArticle nounFrame adjective else ""
          adjective = fun adjFrame
          in do
            args <- adjArgs adjFrame
            return $ negation adjFrame `cat` article `cat` modifiers adjFrame `cat` adjective `cat` args
        adjArgs valFrame = case (attr, getType valFrame) of
          (P.Quality, Just "SIMILAR_TO") -> np False $ fValue P.Arg2 valFrame
          _ -> return ""
        in case value of
          Just _ -> handleSeq adjOrMore value
          Nothing -> return ""
  property <- adjSeq P.Property $ \p -> if hasType "AMAZING" p then "amazing" else ""
  kind <- adjSeq P.Kind $ \p -> case getType p of
    Just "COMMERCIAL" -> "commercial"
    Just "ROASTED" -> "roasted"
    Just "BOILED" -> "boiled"
    _ -> ""
  quality <- adjSeq P.Quality $ \p -> case getType p of
    Just "HUMBLE" -> "humble"
    Just "CLEVER" -> "smart"
    Just "FAST" -> "fast"
    Just "STUPID" -> "stupid"
    Just "SMASHED" | not (isElidedNoun nounFrame) -> "smashed"
    Just "WOVEN" -> "woven"
    Just "BLIND" -> "blind"
    Just "FALL_OUT" -> "falling"
    Just "SIMILAR_TO" -> "like"
    Just "UNILATERAL" -> "unilateral"
    _ -> ""
  color <- adjSeq P.Color $ \p -> case getType p of
    Just "GREEN" -> "green"
    Just "RED" -> "red"
    _ -> ""
  order <- adjSeq P.Order $ \p -> case getType p of
    Just "3" -> "third"
    Just "4" -> "fourth"
    Just "5" -> "fifth"
    Just "6" -> "sixth"
    _ -> ""
  let shopKind = if sValue P.Name nounFrame == Just "гастроном" then "grocery" else ""
  let gender =
        if shouldContrastByGender nounFrame && isHuman nounFrame
        then case sValue P.RusGender nounFrame of
          Just "Masc" -> "male"
          Just "Fem" -> "female"
          _ -> ""
        else ""
  size <- adjSeq P.Size $ \p ->
    if hasType "LITTLE" p then "small"
    else if hasType "EXCESSIVE" p then "excessive"
    else if hasType "UNEMBRACEABLE" p then "unembraceable"
    else if hasType "BIG" p then
      if hasType "GARDEN" nounFrame then "big" else "great"
    else ""
  state <- adjSeq P.State $ \p ->
    if hasType "CLOSED" p then "closed"
    else ""
  return $ order `cat` property `cat` state `cat` kind `cat` shopKind `cat` size `cat` quality `cat` gender `cat` color

shouldContrastByGender frame = case (getType frame, sValue P.RusGender frame) of
  (Just typ, Just gender) | typ /= "NAMED" -> let
    differentGender candidate = case sValue P.RusGender candidate of
      Just g1 -> g1 /= gender
      _ -> False
    contrastible candidate =
      if not (isNumber $ Just candidate) && hasType typ candidate && differentGender candidate then
        case usage P.Arg1 candidate of
          Just fVerb -> not (isVerbEllipsis fVerb) || isEllipsisAnchor (Just candidate) fVerb
          _ -> True
      else False
    in any contrastible $ allFrames $ sense frame
  _ -> False

streetName frame = fromMaybe "" $ fmap streetNameString $ fValue P.VName frame


streetNameString frame = case sValue P.Name frame of
 Just "знаменская" -> "Znamenskaya"
 Just "бассейная" -> "Basseinaya"
 Just "театральная" -> "Teatralnaya"
 Just s -> s
 _ -> ""

fDeterminer frame =
  if hasAnyType ["NEIGHBORS", "AMAZE", "PREDICAMENT", "MOUTH", "NOSE", "JAW", "JAWS", "ARGUE", "FINGER", "SPEECH", "FAMILY", "EYES", "BROTHER", "SISTER", "CORNER", "CURIOSITY", "PLENITUDE"] frame then fValue P.Arg1 frame
  else if hasAnyType ["OPINION"] frame then fValue P.Arg1 frame
  else if hasAnyType ["WORDS", "BOOK"] frame then fValue P.Author frame
  else if hasAnyType ["ROOMS", "APARTMENTS", "OFFICES", "WINDOWS", "FOUNTAIN"] frame then fValue P.Owner frame
  else if hasAnyType ["CASHIER"] frame then fValue P.Place frame
  else if isElidedNoun frame then fValue P.Arg1 frame
  else Nothing

usePronoun state frame = Set.member frame (visitedFrames state)

isHeavyNP :: GenerationState -> Maybe Frame -> Bool
isHeavyNP state mNoun = Just True == fmap isHeavyNoun mNoun where
  isHeavyNoun noun =
    if usePronoun state noun then False
    else if hasType "seq" noun then any (not . isPronoun) (flatten $ Just noun)
    else isJust (fValue P.Relative noun) || isJust (fValue P.VName noun) || isHeavyNP state (fDeterminer noun)

shouldGenerateDeterminer noun det state asSpecifier = let
  prev = filter (\f -> fDeterminer f == Just det) $ prevSiblings noun
  next = filter (\f -> fDeterminer f == Just det) $ nextSiblings noun
  in
  if isNothing (getType det) then False
  else if not (null prev) && Set.member det (visitedFrames state) then False
  else if hasType "OPINION" noun && not (isPronoun det) then not asSpecifier
  else if isHeavyNP state $ Just det then not asSpecifier
  else if any (hasType "WORDS") $ prev ++ next then not asSpecifier
  else if null prev then asSpecifier
  else if null next then not asSpecifier
  else False

determiner frame det nbar = do
  state <- get
  let genitiveSpecifier det =
        let pronoun s = do frameGenerated det; return s
            resolved = resolve det
        in case getType resolved of
          Just s ->
            if isPronoun resolved then pronoun $ genitivePronoun s (isElidedNoun frame)
            else if usePronoun state det then pronoun $ case sValue P.RusGender det of
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
      else if hasAnyType ["SOME", "OTHERS", "THIS", "THAT", "JOY", "RELIEF", "MEANING", "MONEY", "COUNTING", "APARTMENTS", "OFFICES", "HOUSES", "CABBAGE", "CARROT", "MARKET"] frame then ""
      else if hasAnyType ["NAMED"] frame then ""
      else if hasType "OPINION" frame && Just True == fmap isVerbEllipsis (usage P.AccordingTo frame) then ""
      else if isElidedNoun frame && skipElidedOne frame || isPlaceholder frame then ""
      else if sValue P.Given frame == Just "true" then "the"
      else if isArticleAfterAdjectives frame then ""
      else indefiniteArticle frame nbar

indefiniteArticle nounFrame nextText =
  if not $ isSingular nounFrame then ""
  else if any (\c -> [c] `isPrefixOf` nextText) "aeiou8" then "an"
  else "a"

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

mapCat f list = foldM (\s arg -> return s `catM` f arg) "" list

frameGenerated frame = do
  state <- get
  put $ state {
    visitedFrames = Set.insert frame $ visitedFrames state,
    visitedSentenceFrames = Set.insert frame $ visitedSentenceFrames state
  }

sentence :: Frame -> State GenerationState String
sentence frame = handleSeq singleSentence (Just frame) `catM` return (finish ++ newline) where
  singleSentence frame = do
    frameGenerated frame
    let content = fValue P.Content frame
    if Just "object" == sValue P.SituationKind frame
    then np True content
    else fromMaybe (return " ???sentence") $ liftM clause content
  finish = if sValue P.Dot frame == Just "true" || sValue P.Conj (unSeq frame) == Just ";" then "."
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
  Just _ -> do
    let prefix = if isFactCP cp && distinguish cp then "that" else ""
        negation = if Just "true" == sValue P.Negated cp then "not" else ""
    s <- sentence cp
    return $ negation `cat` prefix `cat` stripFirstComma s

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
          Just "SAY" -> return "they say"
          s -> return $ show s
    state <- get
    if Set.member source (visitedFrames state) then return ""
    else return comma `catM` handleSeq oneOpinion (Just source) `catM` return comma
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
    let (_prefixAdjuncts, _postfixAdjuncts) = partition (\(SentenceAdjunct _ _ f) -> typeEarlier f fVerb) $ sentenceAdjuncts fVerb
    prefixAdjuncts <- mapCat generateSentenceAdjunct _prefixAdjuncts
    core <- if isQualityCopula fVerb && Just "true" == (sValue P.ExclamativeQuestion =<< cp)
           then case fSubject >>= getType of
             Just "AMAZE" -> return "Great was" `catM` np True fSubject
             Just "CUNNING_PERSON" -> return "What" `catM` np True fSubject
             _ -> return "??degree"
           else vp fVerb (determineVerbForm fVerb fSubject $ past state) FiniteClause
    elaboration <- case fValue P.Elaboration fVerb of
      Just smth -> do s <- sentence smth; return $ (if hasType "HAPPEN" fVerb then "," else ":") `cat` stripFirstComma s
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
    postfixAdjuncts <- mapCat generateSentenceAdjunct _postfixAdjuncts
    questionVariants <- case cp >>= fValue P.Questioned >>= fValue P.Variants of
      Just variants -> return "-" `catM` np ((cp >>= fValue P.Questioned) == fSubject) (Just variants)
      _ -> return ""
    let noIntro = emphasis `cat` core `cat` controlledComp `cat` postfixAdjuncts `cat` comp `cat` externalComp `cat` questionVariants `cat` elaboration
    return $ prefixAdjuncts `cat` intro `cat` (if null intro then noIntro else stripFirstComma noIntro)

isQuestioned frame = flip any (flatten $ Just frame) $ \frame ->
  hasType "wh" frame ||
  Just True == fmap isQuestioned (fValue P.Arg1 frame) ||
  Just True == fmap isQuestioned (fValue P.Author frame) ||
  Just True == fmap isQuestioned (fValue P.Determiner frame)

data SentenceAdjunct = SentenceAdjunct String String Frame

sentenceAdjuncts fVerb = condComp ++ reasonComp where
  condComp = case () of
    _ | Just fComp <- fValue P.WhenCondition fVerb -> [SentenceAdjunct "," "when" fComp]
    _ | Just fComp <- fValue P.IfCondition fVerb   -> [SentenceAdjunct "," "if" fComp]
    _ | Just caze <- fValue P.Condition fVerb,
        hasType "CASE" caze,
        Just fComp <- msum [fValue P.WhenCondition caze, fValue P.IfCondition caze] -> [SentenceAdjunct "," "only if" fComp]
    _ -> []
  reasonComp = case fValue P.Reason fVerb of
    Just fComp | hasType "situation" fComp -> [SentenceAdjunct "" "because" fComp]
    _ -> []

generateSentenceAdjunct (SentenceAdjunct separator preposition frame) = do
  s <- sentence frame
  return $ separator `cat` preposition `cat` s `cat` separator

data ClauseType = FiniteClause | InfiniteClause deriving (Eq)

shouldElideSubject fVerb fSubject = any hasSameSubject prevVerbSiblings where
  hasSameSubject prevVerb = isTrue $ isSame <$> fSubject <*> englishSubject prevVerb
  isSame f1 f2 = f1 == f2 || getType f1 == getType f2 && hasType "ME" f1
  prevVerbSiblings = mapMaybe (fValue P.Content) $ fromMaybe [] (prevSiblings <$> usage P.Content fVerb)

vp :: Frame -> VerbForm -> ClauseType -> State GenerationState String
vp fVerb verbForm clauseType = do
  let cp = usage P.Content fVerb
      theme = fValue P.Theme fVerb
      isModality = hasType "modality" fVerb
      fSubject = englishSubject fVerb
      isQuestion = Just True == fmap isQuestionCP cp
      nonSubjectQuestion = isQuestion && (isNothing fSubject || not (fromJust fSubject `elem` flatten (cp >>= fValue P.Questioned)))
      inverted = nonSubjectQuestion && Just "true" == (cp >>= sValue P.Question_mark)
      isDoModality = isModality && Just True == fmap (hasType "DO") theme
      thereSubject = clauseType == FiniteClause &&
                     isModality &&
                     (Just "wh" == (fSubject >>= getType) || isNothing (fSubject >>= getType) && Just True /= (fmap isElidedNoun fSubject)) &&
                     not isQuestion
      (_aux, sVerb) = generateVerbs fVerb fSubject verbForm inverted isModality isQuestion isDoModality thereSubject
      aux = if clauseType == InfiniteClause then "" else _aux
      finalAdverb = case getType fVerb of
        Just "HAPPEN" -> "today"
        Just "copula" | isExclamationCopula fVerb && isAtLocationCopula fVerb -> "here"
        Just "MOVE" -> (if Just "SLIGHTLY" == (fValue P.Manner fVerb >>= getType) then "slightly" else "") `cat` "back and forth"
        _ -> ""
      negation = if sValue P.Negated fVerb == Just "true" && isGerund fVerb then "not" else ""
      allArgs = if isModality then fromMaybe [] (fmap arguments theme) else arguments fVerb
      cpPrefixArgs = filter (\a -> argPosition a == BeforeCP) allArgs
      vpPrefixArgs = filter (\a -> argPosition a == BeforeVP) allArgs
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
  subject <-
   if thereSubject then return "there"
   else if Just "WEATHER_BE" == getType fVerb then return "it"
   else if Just "true" == sValue P.Imperative fVerb && isNothing (getDeclaredType =<< fSubject) then return ""
   else case (fSubject, clauseType) of
    (Just f, FiniteClause) ->
      if isVerbEllipsis fVerb && not (reachesEllipsisAnchor fSubject fVerb) then return "it"
      else if isNothing (fSubject >>= getType) && not (isTrue $ isElidedNoun <$> fSubject) then return "someone"
      else if shouldElideSubject fVerb fSubject then
        if (isJust (msum [fValue P.PerfectBackground fVerb, fValue P.Reason fVerb]) || hasType "copula" fVerb)
        then return $ if sValue P.RusGender f == Just "Masc" then "he" else "she"
        else return ""
      else np True fSubject
    _ -> return ""
  beforeCP <- mapCat generateArg cpPrefixArgs
  beforeVP <- mapCat generateArg vpPrefixArgs
  preAdverb <- mapCat generateArg infixArgs
  sArgs <- mapCat generateArg $ Data.List.sortBy (compare `on` argOrder) normalArgs
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
        "are" -> "'re"
        "will" -> "'ll"
        "have" | not (null aux) -> "'ve"
        _ -> ""
  let (afterWh, afterSubject) = if isQuestion && isTrue (isQuestioned <$> fSubject) then ("", beforeVP) else (beforeVP, "")
  let contractableSubject = subject `elem` ["I", "he", "she", "we", "it", "what", "that", "it", "there", "you"]
  let contracted = if not (null shortForm) && null negation && not inverted && contractableSubject && null afterSubject then
                     (subject ++ shortForm) `cat` preAdverb `cat` restVerb
                   else (if inverted then aux `cat` negation `cat` subject else subject `cat` aux `cat` negation) `cat` afterSubject `cat` preAdverb `cat` sVerb
  return $ beforeCP `cat` sTopicalized `cat` whWord `cat` afterWh `cat` contracted `cat` nonWhWord `cat` controlled `cat` sArgs `cat` stranded `cat` verbSuffix fVerb `cat` anymore `cat` finalAdverb

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
      if isJust (getType f) || isElidedNoun f then return (hybridWhPrefix f) `catM` return prep `catM` (np False $ Just f)
      else return ""
    PPAdjunct _ prep f -> return prep `catM` (np False $ Just f)
    ToInfinitive nextVerb -> return "to" `catM` vp nextVerb BaseVerb InfiniteClause
    GerundBackground _ back -> return ("," `cat` conjIntroduction back) `catM` vp back Gerund InfiniteClause `catM` return ","
    GerundArg f -> vp f Gerund InfiniteClause
    AccordingTo _ parent _ -> generateAccording parent
    CommaSurrounded a -> return "," `catM` generateArg a `catM` return ","
    Silence _ -> return ""
    _ -> error $ "Unknown argument " ++ show arg

argOrder arg = case arg of
  PPAdjunct {} -> 2
  Adverb _ "already" -> 0
  NPArg {} -> 0
  CommaSurrounded a -> argOrder a
  _ -> 1