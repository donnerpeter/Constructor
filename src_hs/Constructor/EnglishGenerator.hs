module Constructor.EnglishGenerator (generate) where
import Constructor.Sense
import Control.Monad.State
import Data.List
import Debug.Trace
import Data.Char (toUpper)
import Data.Maybe
import qualified Data.Set as Set

data GenerationState = GenerationState { visitedFrames:: Set.Set Frame, past:: Bool}
data VerbForm = BaseVerb | PastVerb deriving (Eq)

generate:: Sense -> String
generate sense = 
  let topFrames = catMaybes $ map getTopFrame $ allFrames sense
      sentenceState = foldM generateSentence [] topFrames
      generateSentence :: [String] -> Frame -> State GenerationState [String]
      generateSentence output frame = do
        state <- get
        if Set.member frame (visitedFrames state) then return output 
        else do nextSentence <- sentence frame; return $ output++[nextSentence]
      sentences = evalState sentenceState $ GenerationState Set.empty False  
      text = Data.List.intercalate " " $ map capitalize sentences
  in text

capitalize (c:rest) = (toUpper c):rest

getTopFrame frame = if isCP frame then upmostSeq frame else Nothing where 
  isCP frame = hasType "fact" frame || hasType "question" frame
  upmostSeq frame =
    case usage "member1" frame of
      Just p -> upmostSeq p
      _ -> if isJust $ usage "member2" frame then Nothing else Just frame

handleSeq :: (Frame -> State GenerationState String) -> Maybe Frame -> State GenerationState String
handleSeq _ Nothing = return "???"
handleSeq f (Just frame) =
  if hasType "seq" frame then do
      frameGenerated frame
      m1 <- fromMaybe (return "???") $ liftM (handleSeq f . Just) $ fValue "member1" frame 
      m2 <- fromMaybe (return "???") $ liftM f $ fValue "member2" frame
      let conj = fromMaybe "" $ sValue "conj" frame
          separator = if conj == "but" then ", but" else if conj == "" then "," else conj
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
      let n = noun (getType frame)
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

adjectives nounFrame = catMaybes [property, kind, shopKind] where 
  property = sValue "property" nounFrame >>= \p -> if p == "AMAZING" then Just "amazing" else Nothing
  kind = sValue "kind" nounFrame >>= \p -> if p == "COMMERCIAL" then Just "commercial" else Nothing
  shopKind = sValue "name" nounFrame >>= \p -> if p == "гастроном" then Just "grocery" else Nothing

streetName frame = case sValue "name" frame of
 Just "знаменская" -> "Znamenskaya"
 Just "бассейная" -> "Basseinaya"
 Just s -> s
 _ -> ""

determiner frame nbar =
  let det = if hasAnyType ["NEIGHBORS", "AMAZE"] frame then fValue "arg1" frame else Nothing
      genitiveSpecifier det =
        case getType det of
          Just "ME" -> "my"
          Just "HE" -> "his"
          Just "THEY" -> "their"
          Just s -> s
          _ -> "???"
  in
  case det of
    Just _ -> handleSeq (return . genitiveSpecifier) $ det
    _ -> return $
      let sDet = sValue "determiner" frame in
      if sDet == Just "THIS" then "this"
      else if getType frame == Just "STREET" then streetName frame
      else if sValue "number" frame == Just "true" then ""
      else if sValue "given" frame == Just "true" then "the"
      else if "a" `isPrefixOf` nbar then "an"
      else if isSingular (getType frame) then "a"
      else ""

noun Nothing = "??"
noun (Just typ) = case typ of
  "THING" -> "thing"
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
  _ -> typ

isSingular Nothing = False
isSingular (Just typ) = case typ of
  "NEIGHBORS" -> False
  _ -> True

cat "" t2 = t2
cat t1 "" = t1
cat t1 t2 = if "," `isPrefixOf` t2 || "." `isPrefixOf` t2 then  t1 ++ t2 else t1 ++ " " ++ t2

catM :: State GenerationState String -> State GenerationState String -> State GenerationState String
catM t1 t2 = do s1 <- t1; s2 <- t2; return $ s1 `cat` s2

frameGenerated frame = do state <- get; put $ state { visitedFrames = Set.insert frame $ visitedFrames state } 

sentence :: Frame -> State GenerationState String
sentence frame = handleSeq singleSentence $ Just frame where
  singleSentence frame = do
    frameGenerated frame
    let finish = if sValue "dot" frame == Just "true" then "." else ""
    s <- fromMaybe (return "???") $ liftM clause $ fValue "content" frame
    return $ s `cat` finish

genComplement cp = fromMaybe (return "") $ do
  let prefix = if hasType "fact" cp then ", that" else ""
  fVerb <- fValue "content" cp
  if hasType "question" cp && hasType "THINK" fVerb then
    return $ do
      frameGenerated cp
      (return "about their opinion on") `catM` (np False $ fValue "topic" fVerb)
  else return $ do s <- sentence cp; return $ prefix `cat` s

verb verbForm negated typ = case typ of
  "HAPPEN" -> "happened"
  "FORGET" -> "forgot"
  "GO" -> "went"
  "GO_OFF" -> "went"
  "ASK" -> "asked"
  "COME_SCALARLY" -> "comes"
  "DISCOVER" -> "discovered"
  "CAN" -> if negated then "couldn't" else "could"
  "RECALL" -> "recall"
  "REMEMBER" -> if verbForm == PastVerb then "remembered" else "remember"
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
      Just f | [fVerb] `isPrefixOf` (usages "arg1" f) -> np True fSubject
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
    comp <- fromMaybe (return "") $ fmap genComplement $ fComp
    condComp <- case fValue "whenCondition" fVerb of
      Just fComp -> do comp <- sentence fComp; return $ ", when" `cat` comp
      _ -> return ""
    questionVariants <- case fmap (\subj -> (getType subj, fValue "variants" subj)) fSubject of
      Just (Just "wh", Just variants) -> (return "-") `catM` (np True (Just variants))
      _ -> return ""
    return $ core `cat` condComp `cat` comp `cat` questionVariants `cat` elaboration

vp :: Frame -> VerbForm -> String -> State GenerationState String
vp fVerb verbForm subject = do
  let preAdverb = case sValue "manner" fVerb of
        Just "SUDDENLY" -> "suddenly"
        Just s -> s
        _ -> ""
      sVerb = fromMaybe "???" $ fmap (verb verbForm $ Just "true" == sValue "negated" fVerb) $ getType fVerb
      finalAdverb = case getType fVerb of
        Just "HAPPEN" -> "today"
        _ -> ""
  dObj <- case getType fVerb of
    Just "ASK" -> np False $ fValue "arg2" fVerb
    Just "RECALL" -> np False $ fValue "arg2" fVerb
    Just "REMEMBER" -> np False $ fValue "arg2" fVerb
    Just "COME_SCALARLY" -> case fValue "order" fVerb >>= getType of
      Just "EARLIER" -> return "first"
      Just "NEXT" -> return "next"
      _ -> return ""
    _ -> return ""
  io <- case fValue "experiencer" fVerb of
    Just smth -> return "to" `catM` np False (Just smth)
    _ ->
      case fValue "goal" fVerb of
        Just smth -> return "to" `catM` np False (Just smth)
        _ -> return ""  
  controlled <- case getType fVerb of
    Just "CAN" -> case fValue "theme" fVerb of
      Just slave -> vp slave verbForm ""
      _ -> return ""
    _ -> return ""
  location <- case fValue "location" fVerb of
    Just loc -> return "on" `catM` np False (Just loc)
    _ -> return ""
  let contracted = if null preAdverb && sVerb == "is" then subject ++ "'s" else subject `cat` preAdverb `cat` sVerb
  return $ contracted `cat` controlled `cat` dObj `cat` io `cat` location `cat` finalAdverb