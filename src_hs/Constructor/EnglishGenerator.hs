module Constructor.EnglishGenerator (generate) where
import Constructor.Sense
import Control.Monad.State
import Data.List
import Data.Char (toUpper)
import Data.String.Utils (startswith)

generate:: Sense -> String
generate sense = 
  let topFrames = [frame | frame <- allFrames sense, isTopFrame frame]
      text = Data.List.intercalate " " [capitalize (sentence f) | f <- topFrames]
  in text

capitalize (c:rest) = (toUpper c):rest

isTopFrame frame = hasType frame "HAPPEN"-- || hasType frame "FORGET"

np Nothing _ = "???"
np (Just frame) nom =
  if hasType frame "ME" then if nom then "I" else "me"
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
                 else "a" `cat` nbar

noun Nothing = "??"
noun (Just typ) = case typ of
  "THING" -> "thing"
  "ME" -> "me"
  _ -> typ

cat "" t2 = t2
cat t1 "" = t1
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
        Just "COME_SCALARLY" -> "comes first"
        Just s -> s
        Nothing -> "???"
      io = case fValue fVerb "experiencer" of
        Just smth -> cat "to" (np (Just smth) False)
        _ -> ""
      finalAdverb = case getType fVerb of
        Just "HAPPEN" -> "today"
        _ -> ""
      elaboration = case fValue fVerb "elaboration" of
        Just smth -> ":" `cat` (clause smth)
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
  in (np subject True) `cat` preAdverb `cat` verb `cat` io `cat` finalAdverb `cat` comp `cat` questionVariants ++ elaboration
