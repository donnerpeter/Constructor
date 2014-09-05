module Constructor.Issues (Issue, issues) where

import Constructor.Mite
import Constructor.Sense
import Constructor.Util
import Control.Monad
import Data.Maybe
import qualified Constructor.SemanticProperties as P

type Issue = String

issues :: [Mite] -> [Issue]
issues mites = let
  sense = makeSense mites
  frames = allFrames sense
  hasCP = any isCP frames
  incompleteIsolation frame =
    if isJust (sValue P.Isolation frame) && (isNothing (sValue P.LeftIsolated frame) || isNothing (sValue P.RightIsolated frame))
    then ["Incomplete isolation " ++ show frame] else []
  wrongAccording frame =
    if any (not . hasAnyType ["WORDS", "OPINION"]) (flatten $ fValue P.AccordingTo frame)
    then ["invalid accordingTo"] else []
  wrongOptative frame =
    if any (not . hasType "LUCK") (flatten $ fValue P.OptativeModality frame)
    then ["invalid optativeModality"] else []
  wrongNumber frame =
    if hasType "EYES" frame && Just False == fmap (hasType "2") (fValue P.Quantifier frame)
    then ["suspicious eye count"] else []
  frameIssues frame = case getDeclaredType frame of
    Just "seq" | Nothing == sValue P.Conj frame -> ["comma-only seq"]
    Just s | (s == "SIT" || s == "SAY") && isNothing (fValue P.Arg1 frame >>= sDeclaredValue P.Type) ->
      ["unknown " ++ s ++ "subj "]
    Just "ASK" | any isFactCP (flatten $ fValue P.Topic frame) -> ["asking fact"]
    Just s | (s == "WE" || s == "THEY") && isJust (fValue P.Relative frame) ->
      ["relative clause for pronoun"]
    Just "CASHIER" | any (hasType "OTHERS") (flatten $ fValue P.Place frame) -> ["cashier of other people"]
    Just "OPINION" | isNothing (fValue P.Arg1 frame >>= getType) -> ["opinion without subj"]
    Just "WORDS" | isNothing (fValue P.Author frame >>= getType) -> ["words without author"]
    Just s | (s == "FORGET" || s == "THINK") && isNothing (fValue P.Arg2 frame >>= getType) -> [s ++ " without arg2"]
    Just s | (s == "GO" || s == "CAN" || s == "REMEMBER") && Just True /= fmap isAnimate (fValue P.Arg1 frame) -> ["inanimate " ++ s ++ " subject"]
    Just "GO" | Just True == fmap isInanimate (fValue P.RelTime frame >>= fValue P.Anchor) -> ["inanimate GO relTime anchor"]
    Just "COME_SCALARLY" -> let
      fSubj = fValue P.Arg1 frame
      fOrder = fValue P.Order frame
      anchorIssues = if Just True == fmap isAnimate (fOrder >>= fValue P.Anchor) then ["come_scalarly with animate anchor"] else []
      subjIssues = case fSubj of
        Just subj | Nothing == sDeclaredValue P.Type subj -> ["unknown subj"]
        _ -> []
      in anchorIssues ++ subjIssues
    _ -> []
  in {-traceIt "issues" $ -}(frames >>= frameIssues)
    ++ (if hasCP then [] else ["no clause"])
    ++ (frames >>= incompleteIsolation)
    ++ (frames >>= orderingIssues)
    ++ (frames >>= wrongAccording)
    ++ (frames >>= wrongOptative)
    ++ (frames >>= wrongNumber)

orderingIssues frame = case getDeclaredType frame of
  Just "COME_SCALARLY" |
    Just subj <- fValue P.Arg1 frame,
    isJust (sDeclaredValue P.Type subj),
    Just order <- fValue P.Order frame,
    earlier order P.Type subj P.Type && earlier frame P.Type order P.Type ->
      ["come_scalarly order subj"]
  Just "COME_SCALARLY" |
    Just order <- fValue P.Order frame,
    Just relTime <- fValue P.RelTime frame,
    earlier order P.Type relTime P.Type && earlier relTime P.Type frame P.Type ->
      ["order relTime COME_SCALARLY"]
  Just "GO" |
    Just source <- fValue P.Source frame,
    earlier source P.Type frame P.Type ->
      ["source before GO"]
  _ -> []
