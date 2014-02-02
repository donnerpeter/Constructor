module Constructor.EnglishGenerator (generate) where
import Constructor.Sense
import Control.Monad.State
import Data.List
import Debug.Trace
import Data.Char (toUpper)
import Data.Maybe
import qualified Data.Set as Set
import Constructor.Constructions (SemValue(..), Variable(..))

data GenerationState = GenerationState { visitedFrames:: Set.Set Frame, past:: Bool}
data VerbForm = BaseVerb | PastVerb deriving (Eq)

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

isCP frame = hasType "fact" frame || hasType "question" frame
getTopFrame frame = if isCP frame then upmostSeq frame else Nothing where 
  upmostSeq frame =
    case usage "member1" frame of
      Just p -> upmostSeq p
      _ -> if isJust $ usage "member2" frame then Nothing else Just frame

handleSeq :: (Frame -> State GenerationState String) -> Maybe Frame -> State GenerationState String
handleSeq _ Nothing = return "???"
handleSeq f (Just frame) =
  if hasType "seq" frame then do
      frameGenerated frame
      let first = fValue "member1" frame
          second = fValue "member2" frame
      m1 <- fromMaybe (return "???") $ liftM (handleSeq f . Just) first
      state <- get
      let secondProcessed = Just True == fmap (flip Set.member (visitedFrames state)) second
      if secondProcessed then return m1 else do
        m2 <- fromMaybe (return "???") $ liftM f $ second
        let conj = fromMaybe "" $ sValue "conj" frame
            separator = if conj == "but" then ", but"
                        else if conj == "and" && Just True == fmap isCP second && Just True == fmap (hasType "seq") first then ", and"
                        else if conj == "" then "," 
                        else conj
        return $ m1 `cat` separator `cat` m2
  else f frame

np nom frame =
  if (frame >>= getType) == Just "seq" && (frame >>= fValue "member2" >>= getType) == Just "STREET"
  then
    handleSeq (return . streetName) frame `catM` return "streets"
  else handleSeq (np_internal nom True) frame

np_internal :: Bool -> Bool -> Frame -> State GenerationState String
np_internal nom mayHaveDeterminer frame = do
  frameGenerated frame
  unquantified <- if hasType "ME" frame then if nom then return "I" else return "me"
    else if hasType "HE" frame then if nom then return "he" else return "him"
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
         _ -> return nbar1
      genitiveComplement <- case getType frame of
        Just "CORNER" -> case fValue "arg1" frame of
          Just gen -> return "of" `catM` np False (Just gen)
          _ -> return ""
        _ -> return ""
      det <- if mayHaveDeterminer then determiner frame nbar else return ""
      return $ det `cat` nbar `cat` genitiveComplement
  let quantifier = if sValue "quantifier" frame == Just "ALL" then "all" else ""
  relative <- fromMaybe (return "") $ liftM (catM $ return ", the one") $ fmap sentence $ fValue "relative" frame
  return $ unquantified `cat` quantifier `cat` relative

adjectives nounFrame = catMaybes [property, kind, shopKind, size] where 
  property = sValue "property" nounFrame >>= \p -> if p == "AMAZING" then Just "amazing" else Nothing
  kind = sValue "kind" nounFrame >>= \p -> if p == "COMMERCIAL" then Just "commercial" else Nothing
  shopKind = sValue "name" nounFrame >>= \p -> if p == "гастроном" then Just "grocery" else Nothing
  size = sValue "size" nounFrame >>= \p -> if p == "LITTLE" then Just "small" else Nothing

streetName frame = case sValue "name" frame of
 Just "знаменская" -> "Znamenskaya"
 Just "бассейная" -> "Basseinaya"
 Just s -> s
 _ -> ""

determiner frame nbar =
  let det = if hasAnyType ["NEIGHBORS", "AMAZE", "PREDICAMENT", "MOUTH", "NOSE", "JAW", "OPINION"] frame then fValue "arg1" frame else Nothing
      genitiveSpecifier det =
        case getType det of
          Just "ME" -> return "my"
          Just "HE" -> return "his"
          Just "THEY" -> return "their"
          Just "WE" -> return "our"
          Just s -> do
            state <- get
            if Set.member det (visitedFrames state) then return "her" else return s
          _ -> return "???"
  in
  case det of
    Just _ -> handleSeq genitiveSpecifier $ det
    _ -> return $
      let sDet = sValue "determiner" frame in
      if sDet == Just "THIS" then "this"
      else if getType frame == Just "STREET" then streetName frame
      else if sValue "number" frame == Just "true" then ""
      else if sValue "given" frame == Just "true" then "the"
      else if "a" `isPrefixOf` nbar || "e" `isPrefixOf` nbar then "an"
      else if isSingular (getType frame) then "a"
      else ""

noun Nothing _ = "??"
noun (Just typ) frame = case typ of
  "CASE" -> "thing"
  "ME" -> "me"
  "HE" -> "he"
  "NEIGHBORS" -> "neighbors"
  "MATTER" -> "matter"
  "AMAZE" -> "amazement"
  "ORDER" -> "order"
  "COUNTING" -> "counting"
  "CASHIER" -> "cashier"
  "PREDICAMENT" -> "predicament"
  "SHOP" -> "store"
  "CORNER" -> "corner"
  "STREET" -> "street"
  "HAMMER" -> "hammer"
  "MOUTH" -> "mouth"
  "NOSE" -> "nose"
  "OPINION" -> "opinion"
  "7" -> if sValue "number" frame == Just "true" then typ else "seven"
  "8" -> if sValue "number" frame == Just "true" then typ else "eight"
  _ -> typ

isSingular Nothing = False
isSingular (Just typ) = case typ of
  "NEIGHBORS" -> False
  _ -> True

cat "" t2 = t2
cat t1 "" = t1
cat t1 t2 = if "," `isPrefixOf` t2 || "." `isPrefixOf` t2 || ":" `isPrefixOf` t2 then  t1 ++ t2 else t1 ++ " " ++ t2

catM :: State GenerationState String -> State GenerationState String -> State GenerationState String
catM t1 t2 = do s1 <- t1; s2 <- t2; return $ s1 `cat` s2

frameGenerated frame = do state <- get; put $ state { visitedFrames = Set.insert frame $ visitedFrames state } 

sentence :: Frame -> State GenerationState String
sentence frame = handleSeq singleSentence (Just frame) `catM` return finish where
  singleSentence frame = do
    frameGenerated frame
    fromMaybe (return "???") $ liftM clause $ fValue "content" frame
  finish = if sValue "dot" frame == Just "true" then "."
           else if isJust (lastSentence >>= fValue "content" >>= fValue "message") then ":"
           else ""
  lastSentence = if hasType "seq" frame then fValue "member2" frame else Just frame

genComplement cp = fromMaybe (return "") $ do
  let prefix = if hasType "fact" cp then ", that" else ""
  fVerb <- fValue "content" cp
  if hasType "question" cp && hasType "THINK" fVerb then
    return $ do
      frameGenerated cp
      (return "about their opinion on") `catM` (np False $ fValue "topic" fVerb)
  else return $ do s <- sentence cp; return $ prefix `cat` s

verb verbForm frame typ =
  let negated = Just "true" == sValue "negated" frame in
  case typ of
  "HAPPEN" -> "happened"
  "FORGET" -> "forgot"
  "GO" -> "went"
  "GO_OFF" -> "went"
  "ASK" -> if (fValue "topic" frame >>= getType) == Just "PREDICAMENT" then "consult" else "asked"
  "COME_SCALARLY" -> "comes"
  "DISCOVER" -> "discovered"
  "CAN" -> if negated then "couldn't" else "could"
  "RECALL" -> "recall"
  "REMEMBER" -> if verbForm == PastVerb then "remembered" else "remember"
  "SMILE" -> "gave us a sad smile"
  "THANK" -> "thanked"
  "RUN_OUT" -> "ran"
  "TAKE_OUT" -> "took"
  "SAY" -> "said"
  "copula" -> "is"
  _ -> typ

clause :: Frame -> State GenerationState String
clause fVerb = do
    frameGenerated fVerb
    state <- get
    when (sValue "time" fVerb == Just "PAST") (put $ state { past = True })
    state <- get
    let verbForm = if past state then PastVerb else BaseVerb
        fSubject = fValue "arg1" fVerb
    subject <- case fSubject of
      Just f ->
        if [fVerb] `isPrefixOf` (usages "arg1" f) then np True fSubject 
        else if (isJust $ fValue "perfectBackground" fVerb) then return "she"
        else return ""
      _ -> return ""
    opinion <- case fValue "accordingTo" fVerb of
      Just source | hasType "OPINION" source -> return "in" `catM` np False (Just source) `catM` return ","
      _ -> return ""
    core <- if hasType "degree" fVerb && (fromMaybe False $ fmap (hasType "wh") $ fValue "arg2" fVerb)
           then return $ "Great was" `cat` subject
           else vp fVerb verbForm subject
    elaboration <- case fValue "elaboration" fVerb of
      Just smth -> do subClause <- sentence smth; return $ "," `cat` subClause
      _ -> return ""
    let fComp = case getType fVerb of
          Just "FORGET" -> fValue "arg2" fVerb
          Just "ASK" -> fValue "topic" fVerb
          Just "DISCOVER" -> fValue "theme" fVerb
          _ -> Nothing
    background <- case fValue "perfectBackground" fVerb of
      Just back -> return "moving her nose slightly back and forth,"
      _ -> return ""
    comp <- fromMaybe (return "") $ fmap genComplement $ fComp
    externalComp <- if getType fVerb == Just "GO" then 
      case usage "content" fVerb >>= usage "member1" >>= fValue "member2" of
       Just nextClause | (fValue "content" nextClause >>= getType) == Just "ASK" -> do
         frameGenerated nextClause
         return "to" `catM` vp (fromJust $ fValue "content" nextClause) BaseVerb ""
       _ -> return ""
      else return ""
    condComp <- case fValue "whenCondition" fVerb of
      Just fComp -> do comp <- sentence fComp; return $ ", when" `cat` comp
      _ -> case fValue "condition" fVerb of
        Just caze | hasType "CASE" caze -> case fValue "whenCondition" caze of
          Just fComp -> do comp <- sentence fComp; return $ ", only if" `cat` comp
          _ -> return ""
        _ -> return ""  
    questionVariants <- case fmap (\subj -> (getType subj, fValue "variants" subj)) fSubject of
      Just (Just "wh", Just variants) -> (return "-") `catM` (np True (Just variants))
      _ -> return ""
    return $ opinion `cat` background `cat` core `cat` condComp `cat` comp `cat` externalComp `cat` questionVariants `cat` elaboration

vp :: Frame -> VerbForm -> String -> State GenerationState String
vp fVerb verbForm subject = do
  let preAdverb = case sValue "manner" fVerb of
        Just "SUDDENLY" -> "suddenly"
        Just "SADLY" -> if getType fVerb == Just "SMILE" then "" else "sadly"
        Just s -> s
        _ -> ""
      sVerb = fromMaybe "???" $ fmap (verb verbForm fVerb) $ getType fVerb
      finalAdverb = case getType fVerb of
        Just "HAPPEN" -> "today"
        _ -> ""
  controlled <- case getType fVerb of
    Just "CAN" -> case fValue "theme" fVerb of
      Just slave -> vp slave BaseVerb ""
      _ -> return ""
    _ -> return ""
  args <- foldM (\s arg -> return s `catM` generateArg arg) "" (arguments fVerb)
  let contracted = if null preAdverb && sVerb == "is" then subject ++ "'s" else subject `cat` preAdverb `cat` sVerb
  return $ contracted `cat` controlled `cat` args `cat` finalAdverb

data Argument = Adverb String | NPArg Frame | PPArg String Frame

arguments fVerb = reorderArgs $ fromMaybe [] $ flip fmap (getType fVerb) $ \typ ->
  allFrameFacts fVerb >>= \ Fact { attrName = attr, value = semValue} ->
  case semValue of
    VarValue v -> let value = Frame v (sense fVerb) in case (typ, attr) of
      ("COME_SCALARLY", "order") -> case getType value of
        Just "EARLIER" -> [Adverb "first"]
        Just "NEXT" -> [Adverb "next"]
        Just "AFTER" -> case fValue "anchor" value of 
          Just anchor -> [PPArg "after" anchor]
          _ -> [Adverb "after"]
        _ -> []
      ("HAPPEN", "experiencer") -> [PPArg "to" value]
      ("TAKE_OUT", "source") -> [PPArg "out of" value]
      ("RUN_OUT", "source") -> [PPArg "out of" value]
      ("ASK", "topic") -> if hasType "question" value then [] else [PPArg "on" value]
      (_, "goal") -> [PPArg "to" value]
      (_, "mood") -> case getType value of
        Just "JOY" -> [Adverb "cheerfully"]
        Just s -> [Adverb s]
        _ -> []
      (_, "location") -> [PPArg "on" value]
      (_, "arg2") -> if hasType "question" value then [] else [NPArg value]
      _ -> []
    StrValue value -> []
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