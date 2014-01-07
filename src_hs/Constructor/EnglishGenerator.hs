module Constructor.EnglishGenerator (generate) where
import Constructor.Sense
import Control.Monad.State
import Data.List
import Data.Char (toUpper)

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
  else let n = noun (getType frame)
           nbar = case sValue frame "property" of
             Just "AMAZING" -> cat "amazing" n
             _ -> n
           in cat "an" nbar

noun Nothing = "??"
noun (Just typ) = case typ of
  "THING" -> "thing"
  "ME" -> "me"
  _ -> typ

cat "" t2 = t2
cat t1 "" = t1
cat t1 t2 = t1 ++ " " ++ t2

sentence frame = clause frame

clause frame =
  let subject = fValue frame "arg1"
      adverb = case sValue frame "manner" of
        Just "SUDDENLY" -> "suddenly"
        Just s -> s
        _ -> ""
      verb = case getType frame of
        Just "HAPPEN" -> "happened"
        Just "FORGET" -> "forgot"
        Just s -> s
        Nothing -> "???"
      io = case fValue frame "experiencer" of
        Just smth -> cat "to" (np (Just smth) False)
        _ -> ""
      elaboration = case fValue frame "elaboration" of
        Just smth -> ":" `cat` (clause smth)
        _ -> ""
  in (np subject True) `cat` adverb `cat` verb `cat` io ++ elaboration
