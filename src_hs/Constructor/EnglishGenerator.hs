module Constructor.EnglishGenerator (generate) where
import Constructor.Sense
import Control.Monad.State
import Data.List
import Debug.Trace
import Data.Char (toUpper)
import Data.Maybe
import qualified Data.Set as Set

generate:: Sense -> String
generate sense = 
  let topFrames = [frame | frame <- allFrames sense, isTopFrame frame]
      sentenceState :: State (Set.Set Frame) [String]
      sentenceState = foldM generateSentence [] topFrames
      generateSentence :: [String] -> Frame -> State (Set.Set Frame) [String]
      generateSentence output frame = do
        visited <- get
        if Set.member frame visited then return output 
        else do nextSentence <- sentence frame; return $ output++[nextSentence]
      sentences :: [String]
      sentences = evalState sentenceState Set.empty  
      text = Data.List.intercalate " " $ map capitalize sentences
  in text

capitalize (c:rest) = (toUpper c):rest

isCP frame = hasType "fact" frame || hasType "question" frame
isTopFrame frame = isCP frame && (null $ usages "member1" frame) || 
                   hasType "seq" frame && (fromMaybe False $ fmap isCP $ fValue "member1" frame)

handleSeq _ Nothing = "???"
handleSeq f (Just frame) =
  if hasType "seq" frame then
      (f $ fValue "member1" frame) `cat` (fromMaybe "," $ sValue "conj" frame) `cat` (f $ fValue "member2" frame)
  else f $ Just frame

np nom frame = handleSeq (np_internal nom) frame

np_internal _ Nothing = "???"
np_internal nom (Just frame) = 
  if hasType "ME" frame then if nom then "I" else "me"
  else if hasType "HE" frame then if nom then "he" else "him"
  else if hasType "THEY" frame then if nom then "they" else "them"
  else if hasType "wh" frame then "what"
  else let n = noun (getType frame)
           nbar = case sValue "property" frame of
             Just "AMAZING" -> cat "amazing" n
             _ -> n
           in (determiner frame nbar) `cat` nbar
             

determiner frame nbar =
  let det = if hasType "NEIGHBORS" frame || hasType "AMAZE" frame then fValue "arg1" frame else Nothing
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
  _ -> typ

isSingular Nothing = False
isSingular (Just typ) = case typ of
  "NEIGHBORS" -> False
  _ -> True

cat "" t2 = t2
cat t1 "" = t1
cat t1 t2 = if "," `isPrefixOf` t2 then  t1 ++ t2 else t1 ++ " " ++ t2

frameGenerated frame = do visited <- get; put $ Set.insert frame visited 

sentence :: Frame -> State (Set.Set Frame) String
sentence frame = do
  frameGenerated frame
  if hasType "seq" frame then do
    member1 <- fromMaybe (return "???") $ fmap sentence $ fValue "member1" frame
    member2 <- fromMaybe (return "???") $ fmap sentence $ fValue "member2" frame
    return $ member1 `cat` (fromMaybe "," $ sValue "conj" frame) `cat` member2
  else fromMaybe (return "") $ fmap clause $ fValue "content" frame

genComplement cp = fromMaybe (return "") $ do
  fVerb <- fValue "content" cp
  if hasType "question" cp && hasType "THINK" fVerb then
    return $ do
      frameGenerated cp
      return $ "about their opinion on" `cat` (np False $ fValue "topic" fVerb)
  else return $ sentence cp  

clause :: Frame -> State (Set.Set Frame) String
clause fVerb =
  let fSubject = fValue "arg1" fVerb
      preAdverb = case sValue "manner" fVerb of
        Just "SUDDENLY" -> "suddenly"
        Just s -> s
        _ -> ""
      verb = case getType fVerb of
        Just "HAPPEN" -> "happened"
        Just "FORGET" -> "forgot"
        Just "GO_OFF" -> "went"
        Just "ASK" -> "asked"
        Just "COME_SCALARLY" -> "comes first"
        Just s -> s
        Nothing -> "???"
      dObj = case getType fVerb of
        Just "ASK" -> np False $ fValue "arg2" fVerb
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
      questionVariants = case fmap (\subj -> (getType subj, fValue "variants" subj)) fSubject of
        Just (Just "wh", Just variants) -> "-" `cat` (np True (Just variants))
        _ -> ""
      subject = case fSubject of
        Just f | [fVerb] `isPrefixOf` (usages "arg1" f) -> np True fSubject
        _ -> ""
      core = if hasType "degree" fVerb && (fromMaybe False $ fmap (hasType "wh") $ fValue "arg2" fVerb)
             then "Great was" `cat` subject
             else subject `cat` preAdverb `cat` verb `cat` dObj `cat` io `cat` finalAdverb
  in do
    frameGenerated fVerb
    elaboration <- case fValue "elaboration" fVerb of
      Just smth -> do subClause <- sentence smth; return $ "," `cat` subClause
      _ -> return ""
    let fComp = case getType fVerb of
          Just "FORGET" -> fValue "arg2" fVerb
          Just "ASK" -> fValue "topic" fVerb
          _ -> Nothing
    comp <- fromMaybe (return "") $ fmap genComplement $ fComp
    return $ core `cat` comp `cat` questionVariants `cat` elaboration
