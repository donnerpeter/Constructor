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
  let topFrames = [frame | frame <- allFrames sense, isTopFrame frame]
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

isCP frame = hasType "fact" frame || hasType "question" frame
isTopFrame frame = isCP frame && (null $ usages "member1" frame) || 
                   hasType "seq" frame && (fromMaybe False $ fmap isCP $ fValue "member1" frame)

handleSeq _ Nothing = "???"
handleSeq f (Just frame) =
  if hasType "seq" frame then
      (handleSeq f $ fValue "member1" frame) `cat` (fromMaybe "," $ sValue "conj" frame) `cat` (f $ fValue "member2" frame)
  else f $ Just frame

np nom frame = handleSeq (np_internal nom True) frame

np_internal _ _ Nothing = "???"
np_internal nom mayHaveDeterminer (Just frame) = unquantified `cat` quantifier where
  unquantified = if hasType "ME" frame then if nom then "I" else "me"
    else if hasType "HE" frame then if nom then "he" else "him"
    else if hasType "THEY" frame then if nom then "they" else "them"
    else if hasType "WE" frame then if nom then "we" else "us"
    else if hasType "wh" frame then "what"
    else
     let n = noun (getType frame)
         nbar1 = case sValue "property" frame of
           Just "AMAZING" -> cat "amazing" n
           _ -> case sValue "kind" frame of
             Just "COMMERCIAL" -> cat "commercial" n
             _ -> n
         nbar2 = case getType frame of
           Just "ORDER" -> case fValue "arg1" frame of
             Just poss -> (handleSeq (np_internal nom False) $ Just poss) `cat` nbar1
             _ -> nbar1
           _ -> nbar1
         nbar = nbar2
         in if mayHaveDeterminer then (determiner frame nbar) `cat` nbar else nbar
  quantifier = if sValue "quantifier" frame == Just "ALL" then "all" else ""        

determiner frame nbar =
  let det = if hasAnyType ["NEIGHBORS", "AMAZE"] frame then fValue "arg1" frame else Nothing
      genitiveSpecifier det =
        case det >>= getType of
          Just "ME" -> "my"
          Just "HE" -> "his"
          Just "THEY" -> "their"
          Just s -> s
          _ -> "???"
  in
  case det of
    Just _ -> handleSeq genitiveSpecifier $ det
    _ ->
      let sDet = sValue "determiner" frame in
      if sDet == Just "THIS" then "this"
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
  _ -> typ

isSingular Nothing = False
isSingular (Just typ) = case typ of
  "NEIGHBORS" -> False
  _ -> True

cat "" t2 = t2
cat t1 "" = t1
cat t1 t2 = if "," `isPrefixOf` t2 || "." `isPrefixOf` t2 then  t1 ++ t2 else t1 ++ " " ++ t2

frameGenerated frame = do state <- get; put $ state { visitedFrames = Set.insert frame $ visitedFrames state } 

sentence :: Frame -> State GenerationState String
sentence frame = do
  frameGenerated frame
  if hasType "seq" frame then do
    member1 <- fromMaybe (return "???") $ fmap sentence $ fValue "member1" frame
    member2 <- fromMaybe (return "???") $ fmap sentence $ fValue "member2" frame
    let conj = fromMaybe "" $ sValue "conj" frame
        separator = if conj == "but" then ", but" else if conj == "" then "," else conj
    return $ member1 `cat` separator `cat` member2
  else fromMaybe (return "") $ do
    let finish = if sValue "dot" frame == Just "true" then "." else ""
    content <- fValue "content" frame
    return $ do s <- clause content; return $ s `cat` finish

genComplement cp = fromMaybe (return "") $ do
  let prefix = if hasType "fact" cp then ", that" else ""
  fVerb <- fValue "content" cp
  if hasType "question" cp && hasType "THINK" fVerb then
    return $ do
      frameGenerated cp
      return $ "about their opinion on" `cat` (np False $ fValue "topic" fVerb)
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
  _ -> typ

clause :: Frame -> State GenerationState String
clause fVerb =
  let fSubject = fValue "arg1" fVerb
      questionVariants = case fmap (\subj -> (getType subj, fValue "variants" subj)) fSubject of
        Just (Just "wh", Just variants) -> "-" `cat` (np True (Just variants))
        _ -> ""
      subject = case fSubject of
        Just f | [fVerb] `isPrefixOf` (usages "arg1" f) -> np True fSubject
        _ -> ""
      
  in do
    frameGenerated fVerb
    state <- get
    when (sValue "time" fVerb == Just "PAST") (put $ state { past = True })
    state <- get
    let verbForm = if past state then PastVerb else BaseVerb
        core = if hasType "degree" fVerb && (fromMaybe False $ fmap (hasType "wh") $ fValue "arg2" fVerb)
                     then "Great was" `cat` subject
                     else subject `cat` (vp fVerb verbForm)
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
    return $ core `cat` condComp `cat` comp `cat` questionVariants `cat` elaboration

vp fVerb verbForm =
  let preAdverb = case sValue "manner" fVerb of
        Just "SUDDENLY" -> "suddenly"
        Just s -> s
        _ -> ""
      sVerb = fromMaybe "???" $ fmap (verb verbForm $ Just "true" == sValue "negated" fVerb) $ getType fVerb
      dObj = case getType fVerb of
        Just "ASK" -> np False $ fValue "arg2" fVerb
        Just "RECALL" -> np False $ fValue "arg2" fVerb
        Just "REMEMBER" -> np False $ fValue "arg2" fVerb
        Just "COME_SCALARLY" -> case fValue "order" fVerb >>= getType of
          Just "EARLIER" -> "first"
          Just "NEXT" -> "next"
          _ -> ""
        _ -> ""
      io = case fValue "experiencer" fVerb of
        Just smth -> cat "to" (np False (Just smth))
        _ ->
          case fValue "goal" fVerb of
            Just smth -> cat "to" (np False (Just smth))
            _ -> ""  
      finalAdverb = case getType fVerb of
        Just "HAPPEN" -> "today"
        _ -> ""
      controlled = case getType fVerb of
        Just "CAN" -> case fValue "theme" fVerb of
          Just slave -> vp slave verbForm
          _ -> ""
        _ -> ""
  in preAdverb `cat` sVerb `cat` controlled `cat` dObj `cat` io `cat` finalAdverb