module Constructor.Issues where

import Constructor.Mite
import Constructor.Sense
import Constructor.Util
import Control.Monad
import Data.Maybe

type Issue = String

issues :: [Mite] -> [Issue]
issues mites = let
  sense = makeSense mites
  frames = allFrames sense
  hasCP = any (hasAnyType ["fact", "question"]) frames
  frameIssues frame = case getType frame of
    Just "seq" | Nothing == sValue "conj" frame -> ["comma-only seq"]
    Just "SIT" -> if Nothing == (fValue "arg1" frame >>= sDeclaredValue "type") then ["unknown sit subj "] else []
    Just "ASK" -> if any (hasType "fact") (flatten $ fValue "topic" frame) then ["asking fact"] else []
    Just "THEY" -> if isJust $ fValue "relative" frame then ["relative clause for pronoun"] else []
    Just "WE" -> if isJust $ fValue "relative" frame then ["relative clause for pronoun"] else []
    Just "CASHIER" -> if any (hasType "OTHERS") (flatten $ fValue "place" frame) then ["cashier of other people"] else []
    Just "OPINION" -> if isNothing (fValue "arg1" frame >>= getType) then ["opinion without subj"] else []
    Just "WORDS" -> if isNothing (fValue "author" frame >>= getType) then ["words without author"] else []
    Just "COME_SCALARLY" -> case fValue "arg1" frame of
      Just subj ->
        if Nothing == sDeclaredValue "type" subj then ["unknown subj"] else
         case fValue "order" frame of
          Just order | earlier frame "order" subj "type" && earlier frame "type" frame "order" -> ["come_scalarly order subj"]
          _ -> []
    _ -> []
  in {-traceIt "issues" $ -}(frames >>= frameIssues) ++ (if hasCP then [] else ["no clause"])