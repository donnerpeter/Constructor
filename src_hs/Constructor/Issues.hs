module Constructor.Issues (Issue, issues) where

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
  incompleteIsolation frame =
    if isJust (sValue "isolation" frame) && (isNothing (sValue "leftIsolated" frame) || isNothing (sValue "rightIsolated" frame))
    then ["Incomplete isolation " ++ show frame] else []
  wrongAccording frame =
    if any (not . hasAnyType ["WORDS", "OPINION"]) (flatten $ fValue "accordingTo" frame)
    then ["invalid accordingTo"] else []
  wrongOptative frame =
    if any (not . hasType "LUCK") (flatten $ fValue "optativeModality" frame)
    then ["invalid optativeModality"] else []
  frameIssues frame = case getDeclaredType frame of
    Just "seq" | Nothing == sValue "conj" frame -> ["comma-only seq"]
    Just s | (s == "SIT" || s == "SAY") && isNothing (fValue "arg1" frame >>= sDeclaredValue "type") ->
      ["unknown " ++ s ++ "subj "]
    Just "ASK" | any (hasType "fact") (flatten $ fValue "topic" frame) -> ["asking fact"]
    Just s | (s == "WE" || s == "THEY") && isJust (fValue "relative" frame) ->
      ["relative clause for pronoun"]
    Just "CASHIER" | any (hasType "OTHERS") (flatten $ fValue "place" frame) -> ["cashier of other people"]
    Just "OPINION" | isNothing (fValue "arg1" frame >>= getType) -> ["opinion without subj"]
    Just "WORDS" | isNothing (fValue "author" frame >>= getType) -> ["words without author"]
    Just s | (s == "FORGET" || s == "THINK") && isNothing (fValue "arg2" frame >>= getType) -> [s ++ " without arg2"]
    Just s | (s == "GO" || s == "CAN" || s == "REMEMBER") && Just True /= fmap isAnimate (fValue "arg1" frame) -> ["inanimate " ++ s ++ " subject"]
    Just "GO" | Just True == fmap isInanimate (fValue "relTime" frame >>= fValue "anchor") -> ["inanimate GO relTime anchor"]
    Just "COME_SCALARLY" -> let
      fSubj = fValue "arg1" frame
      fOrder = fValue "order" frame
      anchorIssues = if Just True == fmap isAnimate (fOrder >>= fValue "anchor") then ["come_scalarly with animate anchor"] else []
      subjIssues = case fSubj of
        Just subj | Nothing == sDeclaredValue "type" subj -> ["unknown subj"]
        _ -> []
      in anchorIssues ++ subjIssues
    _ -> []
  in {-traceIt "issues" $ -}(frames >>= frameIssues)
    ++ (if hasCP then [] else ["no clause"])
    ++ (frames >>= incompleteIsolation)
    ++ (frames >>= orderingIssues)
    ++ (frames >>= wrongAccording)
    ++ (frames >>= wrongOptative)

orderingIssues frame = case getDeclaredType frame of
  Just "COME_SCALARLY" |
    Just subj <- fValue "arg1" frame,
    isJust (sDeclaredValue "type" subj),
    Just order <- fValue "order" frame,
    earlier order "type" subj "type" && earlier frame "type" order "type" ->
      ["come_scalarly order subj"]
  Just "GO" |
    Just source <- fValue "source" frame,
    earlier source "type" frame "type" ->
      ["source before GO"]
  _ -> []
