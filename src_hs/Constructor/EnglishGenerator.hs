module Constructor.EnglishGenerator (generate) where
import Constructor.Sense
import Control.Monad.State
import Data.List
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

isTopFrame frame = hasType "fact" frame || hasType "question" frame

np Nothing _ = "???"
np (Just frame) nom =
  if hasType "SEQ" frame then
    (np (fValue "member1" frame) nom) `cat` (fromMaybe "," $ sValue "conj" frame) `cat` (np (fValue "member2" frame) nom)
  else if hasType "ME" frame then if nom then "I" else "me"
  else if hasType "HE" frame then if nom then "He" else "him"
  else if hasType "WH" frame then "what"
  else let n = noun (getType frame)
           nbar = case sValue "property" frame of
             Just "AMAZING" -> cat "amazing" n
             _ -> n
           in (determiner frame nbar) `cat` nbar
             

determiner frame nbar =
  let det = if hasType "NEIGHBORS" frame then fValue "arg1" frame else Nothing in
  case det >>= getType of
    Just "ME" -> "my"
    Just "HE" -> "his"
    _ ->
      if sValue "number" frame == Just "true" then ""
      else if "a" `isPrefixOf` nbar then "an"
      else if isSingular (getType frame) then "a"
      else ""

noun Nothing = "??"
noun (Just typ) = case typ of
  "THING" -> "thing"
  "ME" -> "me"
  "HE" -> "he"
  "NEIGHBORS" -> "neighbors"
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
  fromMaybe (return "") $ fmap clause $ fValue "content" frame

clause :: Frame -> State (Set.Set Frame) String
clause fVerb =
  let subject = fValue "arg1" fVerb
      preAdverb = case sValue "manner" fVerb of
        Just "SUDDENLY" -> "suddenly"
        Just s -> s
        _ -> ""
      verb = case getType fVerb of
        Just "HAPPEN" -> "happened"
        Just "FORGET" -> "forgot"
        Just "GO_OFF" -> "went"
        Just "COME_SCALARLY" -> "comes first"
        Just s -> s
        Nothing -> "???"
      io = case fValue "experiencer" fVerb of
        Just smth -> cat "to" (np (Just smth) False)
        _ ->
          case fValue "goal" fVerb of
            Just smth -> cat "to" (np (Just smth) False)
            _ -> ""  
      finalAdverb = case getType fVerb of
        Just "HAPPEN" -> "today"
        _ -> ""
      questionVariants = case fmap (\subj -> (getType subj, fValue "variants" subj)) subject of
        Just (Just "WH", Just variants) -> "-" `cat` (np (Just variants) True)
        _ -> ""
  in do
    frameGenerated fVerb
    elaboration <- case fValue "elaboration" fVerb of
      Just smth -> do subClause <- sentence smth; return $ "," `cat` subClause
      _ -> return ""
    let fComp = case getType fVerb of
          Just "FORGET" -> fValue "arg2" fVerb
          _ -> Nothing
    comp <- fromMaybe (return "") $ fmap sentence $ fComp
    return $ (np subject True) `cat` preAdverb `cat` verb `cat` io `cat` finalAdverb `cat` comp `cat` questionVariants `cat` elaboration
