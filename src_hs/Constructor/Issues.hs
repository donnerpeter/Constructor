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
  wrongLocation frame =
    if hasType "CASE" frame && isJust (msum $ map (flip usage frame) [P.Location, P.Location_on, P.Location_in])
    then ["wrong location"] else []
  frameIssues frame = case getDeclaredType frame of
    Just "seq" | Nothing == sValue P.Conj frame -> ["comma-only seq"]
    Just s | (s == "SIT" || s == "SAY") && isNothing (fValue P.Arg1 frame >>= sDeclaredValue P.Type) ->
      ["unknown " ++ s ++ "subj "]
    Just "ASK" | any isFactCP (flatten $ fValue P.Topic frame) -> ["asking fact"]
    Just s | (s == "WE" || s == "THEY") && isJust (fValue P.Relative frame) ->
      ["relative clause for pronoun/number"]
    Just _ | Just relativeCP <- fValue P.Relative frame,
             Just wh <- fValue P.Questioned relativeCP,
             Just wh == (fValue P.Content relativeCP >>= fValue P.VTime) ->
      ["time relative clause"]
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
    ++ (frames >>= wrongLocation)

orderingIssues frame = case getDeclaredType frame of
  Just "COME_SCALARLY" |
    Just subj <- fValue P.Arg1 frame,
    isJust (sDeclaredValue P.Type subj),
    Just order <- fValue P.Order frame,
    typeEarlier order subj && typeEarlier frame order ->
      ["come_scalarly order subj"]
  Just "COME_SCALARLY" |
    Just order <- fValue P.Order frame,
    Just relTime <- fValue P.RelTime frame,
    typeEarlier order relTime && typeEarlier relTime frame ->
      ["order relTime COME_SCALARLY"]
  Just "GO" |
    Just source <- fValue P.Source frame,
    typeEarlier source frame ->
      ["source before GO"]
  _ -> []
