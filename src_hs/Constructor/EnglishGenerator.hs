module Constructor.EnglishGenerator (generate) where
import Constructor.Sense
import Control.Monad.State
import Data.List
import Data.Char (toUpper)
import Data.String.Utils (startswith)
import Data.Maybe

generate:: Sense -> String
generate sense = 
  let topFrames = [frame | frame <- allFrames sense, isTopFrame frame]
      text = Data.List.intercalate " " [capitalize (sentence f) | f <- topFrames]
  in text

capitalize (c:rest) = (toUpper c):rest

isTopFrame frame = hasType frame "HAPPEN" || hasType frame "GO_OFF"

np Nothing _ = "???"
np (Just frame) nom =
  if hasType frame "SEQ" then
    (np (fValue frame "member1") nom) `cat` (fromMaybe "," $ sValue frame "conj") `cat` (np (fValue frame "member2") nom)
  else if hasType frame "ME" then if nom then "I" else "me"
  else if hasType frame "WH" then "what"
  else let n = noun (getType frame)
           nbar = case sValue frame "property" of
             Just "AMAZING" -> cat "amazing" n
             _ -> n
           in
             case sValue frame "number" of
               Just "true" -> nbar
               _ ->
                 if startswith "a" nbar then "an" `cat` nbar
                 else if isSingular (getType frame) then "a" `cat` nbar else nbar

noun Nothing = "??"
noun (Just typ) = case typ of
  "THING" -> "thing"
  "ME" -> "me"
  "NEIGHBORS" -> "neighbors"
  _ -> typ

isSingular Nothing = False
isSingular (Just typ) = case typ of
  "NEIGHBORS" -> False
  _ -> True

cat "" t2 = t2
cat t1 "" = t1
cat t1 (',':t2) = t1 ++ "," ++ t2
cat t1 t2 = t1 ++ " " ++ t2

sentence frame = clause frame

clause fVerb =
  let subject = fValue fVerb "arg1"
      preAdverb = case sValue fVerb "manner" of
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
      io = case fValue fVerb "experiencer" of
        Just smth -> cat "to" (np (Just smth) False)
        _ ->
          case fValue fVerb "goal" of
            Just smth -> cat "to" (np (Just smth) False)
            _ -> ""  
      finalAdverb = case getType fVerb of
        Just "HAPPEN" -> "today"
        _ -> ""
      elaboration = case fValue fVerb "elaboration" of
        Just smth -> "," `cat` (clause smth)
        _ -> ""
      fComp = case getType fVerb of
        Just "FORGET" -> fValue fVerb "arg2"
        _ -> Nothing
      comp = case fComp >>= \cp -> fValue cp "content" of
        Just nested -> clause nested
        Nothing -> ""
      questionVariants = case subject >>= \subj -> Just (getType subj, fValue subj "variants") of
        Just (Just "WH", Just variants) -> "-" `cat` (np (Just variants) True)
        _ -> ""
  in (np subject True) `cat` preAdverb `cat` verb `cat` io `cat` finalAdverb `cat` comp `cat` questionVariants `cat` elaboration
