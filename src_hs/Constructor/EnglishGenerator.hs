module Constructor.EnglishGenerator (generate) where
import Constructor.Sense
import Control.Monad.State
import Data.List
import Debug.Trace
import Data.Char (toUpper)
import Data.Maybe
import qualified Data.Set as Set
import Constructor.Variable

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
  in text

capitalize (c:rest) = (toUpper c):rest

isCP frame = hasAnyType ["fact", "question"] frame
isCPOrSeq frame = any isCP $ flatten $ Just frame
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
                          else ", but"
                        else if conj == "and" && Just True == fmap isCP second && Just True == fmap (hasType "seq") first then ", and"
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
  else handleSeq (np_internal nom (if isNumber frame then isSubj || isAnchor else True)) frame where
  isSeq = (frame >>= getType) == Just "seq"
  isSubj = isJust (frame >>= usage "arg1")
  isAnchor = isJust (frame >>= usage "anchor")

np_internal :: Bool -> Bool -> Frame -> State GenerationState String
np_internal nom mayHaveDeterminer frame = do
  frameGenerated frame
  unquantified <- if hasType "ME" frame then if nom then return "I" else return "me"
    else if hasType "HE" frame then if nom then return "he" else return "him"
    else if hasType "SHE" frame then if nom then return "she" else return "her"
    else if hasType "THEY" frame then if nom then return "they" else return "them"
    else if hasType "WE" frame then if nom then return "we" else return "us"
    else if hasType "wh" frame then return $ if isJust $ usage "arg1" frame >>= usage "content" >>= usage "relative"  then "that" else "what"
    else do
      let n = noun (getType frame) frame
          adjs = foldl cat "" $ adjectives frame
          nbar1 = adjs `cat` n
      nbar <- case getType frame of
         Just "ORDER" -> case fValue "arg1" frame of
           Just poss -> handleSeq (np_internal True False) (Just poss) `catM` return nbar1
           _ -> return nbar1
         _ -> case fValue "location" frame of
           Just loc -> return nbar1 `catM` return "on" `catM` np False (Just loc)
           _ -> return nbar1
      genitiveComplement <- case getType frame of
        Just "CORNER" -> case fValue "arg1" frame of
          Just gen -> return "of" `catM` np False (Just gen)
          _ -> return ""
        Just "OPINION" | not $ isDeterminerOpinion frame -> case fValue "arg1" frame of
          Just gen -> return "of" `catM` np False (Just gen)
          _ -> return ""
        _ -> return ""
      det <- if mayHaveDeterminer then determiner frame nbar else return ""
      return $ det `cat` nbar `cat` genitiveComplement
  let postQuantifier = if (fValue "quantifier" frame >>= getType) == Just "ALL" || 
                          (usage "arg1" frame >>= getType) == Just "DISPERSE"
                       then "all" else ""
  let preQuantifier = if (fValue "quantifier" frame >>= getType) == Just "BOTH" then "both of" else ""
  relative <- fromMaybe (return "") $ liftM (catM $ return ", the one") $ fmap sentence $ fValue "relative" frame
  return $ preQuantifier `cat` unquantified `cat` postQuantifier `cat` relative

adjectives nounFrame = catMaybes [property, kind, shopKind, size] where 
  property = fValue "property" nounFrame >>= getType >>= \p -> if p == "AMAZING" then Just "amazing" else Nothing
  kind = fValue "kind" nounFrame >>= getType >>= \p -> if p == "COMMERCIAL" then Just "commercial" else Nothing
  shopKind = sValue "name" nounFrame >>= \p -> if p == "гастроном" then Just "grocery" else Nothing
  size = fValue "size" nounFrame >>= getType >>= \p -> if p == "LITTLE" then Just "small" else Nothing

streetName frame = case sValue "name" frame of
 Just "знаменская" -> "Znamenskaya"
 Just "бассейная" -> "Basseinaya"
 Just "театральная" -> "Teatralnaya"
 Just s -> s
 _ -> ""

isDeterminerOpinion frame = all (hasAnyType ["ME", "THEY"]) (flatten $ fValue "arg1" frame)
determiner frame nbar =
  let det = if hasAnyType ["NEIGHBORS", "AMAZE", "PREDICAMENT", "MOUTH", "NOSE", "JAW", "ARGUE"] frame then fValue "arg1" frame
            else if hasAnyType ["OPINION"] frame && isDeterminerOpinion frame then fValue "arg1" frame
            else if hasAnyType ["WORDS"] frame then fValue "author" frame
            else Nothing
      genitiveSpecifier det =
        case getType det of
          Just "ME" -> return "my"
          Just "HE" -> return "his"
          Just "THEY" -> return "their"
          Just "WE" -> return "our"
          Just "SHE" -> return "her"
          Just s -> do
            state <- get
            if Set.member det (visitedFrames state) then return $ case sValue "rusGender" det of
               Just "Masc" -> "his"
               Just "Fem" -> "her"
               Just "Neu" -> "its"
               _ -> s
            else return s
          _ -> return "???det"
  in
  case det of
    Just _ -> handleSeq genitiveSpecifier $ fmap resolve det
    _ -> return $
      let sDet = fValue "determiner" frame >>= getType in
      if sDet == Just "THIS" then "this"
      else if sDet == Just "ANY" then "any"
      else if hasType "STREET" frame then streetName frame
      else if hasAnyType ["SOME", "OTHERS", "THIS", "THAT"] frame then ""
      else if hasType "OPINION" frame && Just True == fmap isVerbEllipsis (usage "accordingTo" frame) then ""
      else if sValue "given" frame == Just "true" then "the"
      else if "a" `isPrefixOf` nbar || "e" `isPrefixOf` nbar || "8" `isPrefixOf` nbar then "an"
      else if isSingular (getType frame) then "a"
      else ""

isVerbEllipsis verb = Just "true" == (usage "content" verb >>= sValue "ellipsis")

noun Nothing _ = "??"
noun (Just typ) frame = case typ of
  "CASE" -> "thing"
  "ME" -> "me"
  "HE" -> "he"
  "NEIGHBORS" -> "neighbors"
  "TREES" -> "trees"
  "MATTER" -> "matter"
  "AMAZE" -> "amazement"
  "ORDER" -> "order"
  "COUNTING" -> "counting"
  "CASHIER" -> "cashier"
  "WORDS" -> "words"
  "PREDICAMENT" -> "predicament"
  "SHOP" -> "store"
  "CORNER" -> "corner"
  "STREET" -> "street"
  "HAMMER" -> "hammer"
  "MOUTH" -> "mouth"
  "NOSE" -> "nose"
  "OPINION" -> "opinion"
  "MEANING" -> "meaning"
  "SOME" -> "some"
  "OTHERS" -> "others"
  "CHILD" -> "child"
  "BENCH" -> "bench"
  "JAW" -> "jaws"
  "ARGUE" -> "argument"
  "THIS" -> "that"
  "GARDEN" -> if (fValue "name" frame >>= getType) == Just "летний" then "Summer Garden" else "garden"
  "7" -> if sValue "number" frame == Just "true" then typ else "seven"
  "8" -> if sValue "number" frame == Just "true" then typ else "eight"
  _ -> typ

isSingular Nothing = False
isSingular (Just typ) = case typ of
  "NEIGHBORS" -> False
  "TREES" -> False
  _ -> True

cat "" t2 = t2
cat t1 "" = t1
cat t1 t2 = if "," `isPrefixOf` t2 || "." `isPrefixOf` t2 || ":" `isPrefixOf` t2 || ";" `isPrefixOf` t2 then  t1 ++ t2 else t1 ++ " " ++ t2

catM :: State GenerationState String -> State GenerationState String -> State GenerationState String
catM t1 t2 = do s1 <- t1; s2 <- t2; return $ s1 `cat` s2

frameGenerated frame = do state <- get; put $ state { visitedFrames = Set.insert frame $ visitedFrames state } 

sentence :: Frame -> State GenerationState String
sentence frame = handleSeq singleSentence (Just frame) `catM` return finish where
  singleSentence frame = do
    frameGenerated frame
    fromMaybe (return "???sentence") $ liftM clause $ fValue "content" frame
  finish = if sValue "dot" frame == Just "true" then "."
           else if isJust (lastSentence >>= fValue "content" >>= fValue "message") then ":"
           else ""
  lastSentence = if hasType "seq" frame then fValue "member2" frame else Just frame

genComplement cp = case fValue "content" cp of
  Nothing -> return ""
  Just fVerb -> let
      prefix = if hasType "fact" cp && (isNothing (usage "member2" cp) || Just "true" == sValue "distinguished" cp) then "that" else ""
      negation = if Just "true" == sValue "negated" cp then "not" else ""
    in return (negation `cat` prefix) `catM` sentence cp

isGerund fVerb = hasAnyType ["SIT", "THINK"] fVerb

verb verbForm frame typ =
  let negated = Just "true" == sValue "negated" frame in
  case typ of
  "HAPPEN" -> "happened"
  "FORGET" -> "forgot"
  "DO" -> "did"
  "GO" -> "went"
  "GO_OFF" -> "went"
  "ASK" -> if (fValue "topic" frame >>= getType) == Just "PREDICAMENT" then if verbForm == PastVerb then "consulted" else "consult" else "asked"
  "COME_SCALARLY" -> if sValue "time" frame == Just "PAST" then "went" else "comes"
  "DISCOVER" -> "discovered"
  "DISTRACT" -> "distracted"
  "DISPERSE" -> "went"
  "THINK" -> "thinking"
  "SIT" -> "sitting"
  "FALL" -> "fell"
  "BREAK" -> "broke"
  "STOP" -> "stopped"
  "CAN" -> if negated then "couldn't" else "could"
  "BEGIN" -> "started"
  "COUNT" -> if verbForm == Gerund then "counting" else "count"
  "ARGUE" -> if verbForm == Gerund then "arguing" else if Just "true" == sValue "irrealis" frame then "were arguing" else "argue"
  "RECALL" -> "recall"
  "REMEMBER" -> if verbForm == PastVerb then "remembered" else if verbForm == Sg3Verb then "remembers" else "remember"
  "SMILE" -> "gave us a " ++ (if sValue "manner" frame == Just "SADLY" then "sad " else "") ++ "smile"
  "THANK" -> "thanked"
  "RUN_OUT" -> "ran"
  "TAKE_OUT" -> "took"
  "GET_SAD" -> "got sad"
  "SAY" -> "said"
  "LACK" -> "were void of"
  "copula" -> if Just "ME" == (fValue "arg1" frame >>= getType) then "am" else if sValue "time" frame == Just "PAST" then "was" else "is"
  _ -> typ

clause :: Frame -> State GenerationState String
clause fVerb = do
    frameGenerated fVerb
    state <- get
    when (sValue "time" fVerb == Just "PAST") (put $ state { past = True })
    state <- get
    let emphasis = cat (if sValue "butEmphasis" fVerb == Just "true" then "but"
                        else if sValue "andEmphasis" fVerb == Just "true" then "and"
                        else "") 
                       (if (fValue "optativeModality" fVerb >>= getType) == Just "LUCK" then "by some sheer luck,"
                        else if sValue "emphasis" fVerb == Just "true" then "there,"
                        else if sValue "relTime" fVerb == Just "AFTER" then "then"
                        else "")
    let verbForm = if past state then PastVerb else if Just True == fmap (hasAnyType ["HE", "SHE"]) fSubject then Sg3Verb else BaseVerb
        fSubject = fValue "arg1" fVerb
    subject <- case fSubject of
      Just f ->
        if [fVerb] `isPrefixOf` (usages "arg1" f) then np True fSubject 
        else if (isJust $ fValue "perfectBackground" fVerb) then return "she"
        else return ""
      _ -> return ""
    opinion <- case fValue "accordingTo" fVerb of
      Just source | hasType "OPINION" source -> return "in" `catM` np False (Just source) `catM` return (if isVerbEllipsis fVerb then "" else ",")
      _ -> return ""
    core <- if hasType "degree" fVerb && (fromMaybe False $ fmap (hasType "wh") $ fValue "arg2" fVerb)
           then return $ "Great was" `cat` subject
           else if hasType "modality" fVerb then return "What were we supposed to do?"
           else vp fVerb verbForm subject
    elaboration <- case fValue "elaboration" fVerb of
      Just smth -> return (if hasType "HAPPEN" fVerb then "," else ":") `catM` sentence smth
      _ -> return ""
    let fComp = case getType fVerb of
          Just "FORGET" -> fValue "arg2" fVerb
          Just "ASK" -> fValue "topic" fVerb
          Just "DISCOVER" -> fValue "theme" fVerb
          _ -> Nothing
    background <- case fValue "perfectBackground" fVerb of
      Just back -> case getType back of
        -- todo vary perfectBackground constituents
        Just "MOVE" -> return "moving her nose slightly back and forth,"
        Just "THINK" -> return "thinking carefully about cashier's words,"
        Just "COME_TO" -> return "but reaching a six in count,"
        _ -> return ""
      _ -> return ""
    comp <- case fComp of
      Nothing -> return "" 
      Just cp -> let compVerb = fValue "content" cp in
        if hasType "question" cp && Just True == fmap (hasType "THINK") compVerb then
           do
             frameGenerated cp
             (return "about their opinion on") `catM` (np False $ fValue "topic" $ fromJust compVerb)
        else return (if hasType "fact" $ head $ flatten fComp then "," else "") `catM` handleSeq genComplement fComp
    externalComp <- if getType fVerb == Just "GO" then 
      case usage "content" fVerb >>= usage "member1" >>= fValue "member2" of
       Just nextClause | (fValue "content" nextClause >>= getType) == Just "ASK" -> do
         frameGenerated nextClause
         return "to" `catM` vp (fromJust $ fValue "content" nextClause) BaseVerb ""
       _ -> return ""
      else return ""
    condComp <- case fValue "whenCondition" fVerb of
      Just fComp -> return ", when" `catM` sentence fComp
      _ -> case fValue "condition" fVerb of
        Just caze | hasType "CASE" caze -> case fValue "whenCondition" caze of
          Just fComp -> do comp <- sentence fComp; return $ ", only if" `cat` comp
          _ -> return ""
        _ -> return ""  
    reasonComp <- case fValue "reason" fVerb of
      Just fComp -> if (fValue "content" fComp >>= getType) == Just "SEEM" && isJust (fValue "content" fComp >>= fValue "theme")
        then do frameGenerated fComp; return "because" `catM` clause (fromJust $ fValue "content" fComp >>= fValue "theme")
        else return "because" `catM` sentence fComp
      _ -> return ""
    questionVariants <- case fmap (\subj -> (getType subj, fValue "variants" subj)) fSubject of
      Just (Just "wh", Just variants) -> (return "-") `catM` (np True (Just variants))
      _ -> return ""
    return $ emphasis `cat` opinion `cat` background `cat` core `cat` condComp `cat` reasonComp `cat` comp `cat` externalComp `cat` questionVariants `cat` elaboration

isQuestioned frame = hasType "wh" frame

vp :: Frame -> VerbForm -> String -> State GenerationState String
vp fVerb verbForm subject = do
  let preAdverb = case sValue "manner" fVerb of
        Just "SUDDENLY" -> "suddenly"
        Just "JUST" -> "just"
        Just "SADLY" -> if getType fVerb == Just "SMILE" then "" else "sadly"
        Just s -> s
        _ -> ""
      sVerb = if isVerbEllipsis fVerb then "did" else fromMaybe "???vp" $ fmap (verb verbForm fVerb) $ getType fVerb
      finalAdverb = case getType fVerb of
        Just "HAPPEN" -> "today"
        _ -> ""
      whWord = if Just True == (fmap isQuestioned $ fValue "arg2" fVerb) then "what" else ""
      negation = if sValue "negated" fVerb == Just "true" && isGerund fVerb then "not" else ""
      aux = if isGerund fVerb && isNothing (usage "content" fVerb >>= usage "member2") then
              if Just "Pl" == (fValue "arg1" fVerb >>= sValue "rusNumber") then "were" else "was"
            else ""
  controlled <- case getType fVerb of
    Just "CAN" -> case fValue "theme" fVerb of
      Just slave -> vp slave BaseVerb ""
      _ -> return ""
    Just "BEGIN" -> case fValue "theme" fVerb of
      Just slave -> vp slave Gerund ""
      _ -> return ""
    _ -> return ""
  args <- foldM (\s arg -> return s `catM` generateArg arg) "" (arguments fVerb)
  let contracted = if null preAdverb && null negation && null aux then
                     if sVerb == "am" then subject ++ "'m"
                     else if sVerb == "is" then subject ++ "'s"
                     else subject `cat` sVerb
                   else subject `cat` aux `cat` negation `cat` preAdverb `cat` sVerb
  return $ whWord `cat` contracted `cat` controlled `cat` args `cat` finalAdverb

data Argument = Adverb String | NPArg Frame | PPArg String Frame

arguments fVerb = reorderArgs $ fromMaybe [] $ flip fmap (getType fVerb) $ \typ ->
  allFrameFacts fVerb >>= \ Fact { attrName = attr, value = semValue} ->
  case semValue of
    VarValue v -> let value = Frame v (sense fVerb) in case (typ, attr) of
      ("COME_SCALARLY", "order") -> case getType value of
        Just "EARLIER" -> [Adverb "first"]
        Just "NEXT" -> if isVerbEllipsis fVerb then [] else [Adverb "next"]
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
      ("ASK", "topic") -> if all (hasType "question") $ flatten $ Just value then [] else [PPArg "on" value]
      ("LACK", "theme") -> [NPArg value]
      ("DISTRACT", "theme") -> [PPArg "from" value]
      ("DISPERSE", "goal") -> if hasType "HOMES" value then [Adverb "home"] else [PPArg "to" value]
      (_, "goal") -> [PPArg "to" value]
      (_, "mood") -> case getType value of
        Just "JOY" -> [Adverb "cheerfully"]
        Just s -> [Adverb s]
        _ -> []
      (_, "location") -> [PPArg "on" value]
      (_, "arg2") -> if isCPOrSeq value || isQuestioned value then [] else [NPArg value]
      _ -> []
    StrValue value -> case (attr, value) of
      ("anchor", "AGAIN") -> [Adverb "again"]
      ("duration", "LONG") -> [Adverb "for a long time"]
      _ -> []
  where
  isNPArg arg = case arg of
    NPArg {} -> True
    _ -> False
  reorderArgs args = filter isNPArg args ++ filter (not . isNPArg) args 

generateArg :: Argument -> State GenerationState String
generateArg arg = case arg of
  Adverb s -> return s
  NPArg f -> np False $ Just f
  PPArg prep f -> return prep `catM` (np False $ Just f)