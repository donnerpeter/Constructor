module Constructor.Issues (Issue, issues) where

import Constructor.Mite
import Constructor.Sense
import Constructor.Util
import Constructor.Variable
import Control.Monad
import Data.Maybe
import qualified Constructor.SemanticProperties as P

type Issue = String

issues :: Sense -> [Issue]
issues sense = let
  frames = allFrames sense
  hasCP = any isCP frames
  factIssues = facts sense >>= \fact -> let
    frame = Frame (variable fact) sense
    in case value fact of
      StrValue attr val -> case (attr, val) of
        (P.Isolation, _) | (isNothing (sValue P.LeftIsolated frame) || isNothing (sValue P.RightIsolated frame)) ->
          ["Incomplete isolation " ++ show frame]
        (P.Type, declaredType) -> typeIssues frame declaredType ++ orderingIssues frame declaredType
        _ -> []
      VarValue attr val -> let
        valFrame = Frame val sense
        in case attr of
          P.AccordingTo | any (not . hasAnyType ["WORDS", "OPINION"]) (flatten $ Just valFrame) -> ["invalid accordingTo"]
          P.OptativeModality | any (not . hasAnyType ["LUCK"]) (flatten $ Just valFrame) -> ["invalid optativeModality"]
          P.Quantifier | hasType "EYES" frame && not (hasType "2" valFrame) -> ["suspicious eye count"]

          P.Condition | not (hasType "CASE" valFrame) -> ["wrong condition"]
          P.Relative | Just wh <- fValue P.Questioned valFrame ->
            if hasAnyType ["WE", "THEY"] frame then ["relative clause for pronoun/number"]
            else if Just wh == (fValue P.Content valFrame >>= fValue P.VTime) then ["time relative clause"]
            else []
          P.Topic | hasType "ASK" frame && any isFactCP (flatten $ Just valFrame) -> ["asking fact"]

          _ | attr `elem` [P.Location, P.Location_on, P.Location_in] && hasAnyType ["CASE", "COUNTING"] valFrame -> ["wrong location"]
          _ -> []
  typeIssues frame declaredType = case declaredType of
    "seq" | Nothing == sValue P.Conj frame -> ["comma-only seq"]
    s | (s == "SIT" || s == "SAY" || s == "FORGET") && isNothing (fValue P.Arg1 frame >>= sDeclaredValue P.Type) ->
      ["unknown " ++ s ++ "subj "]
    "CASHIER" | any (hasType "OTHERS") (flatten $ fValue P.Place frame) -> ["cashier of other people"]
    "OPINION" | isNothing (fValue P.Arg1 frame >>= getType) -> ["opinion without subj"]
    "WORDS" | isNothing (fValue P.Author frame >>= getType) -> ["words without author"]
    s | (s == "FORGET" || s == "THINK") && isNothing (fValue P.Arg2 frame >>= getType) -> [s ++ " without arg2"]
    s | (s == "GO" || s == "CAN" || s == "REMEMBER" || s == "KNOW" || s == "copula_talking_about") &&
             not (and $ map isAnimate $ flatten $ fValue P.Arg1 frame) -> ["inanimate " ++ s ++ " subject"]
    "copula_about" | (or $ map isAnimate $ flatten $ fValue P.Arg1 frame) -> ["animate " ++ declaredType ++ " subject"]
    "GO" | Just True == fmap isInanimate (fValue P.RelTime frame >>= fValue P.Anchor) -> ["inanimate GO relTime anchor"]
    "WEATHER_BE" | Just True /= fmap (hasAnyType ["SNOW", "RAIN"]) (fValue P.Arg1 frame) -> ["non-weather weather_be"]
    "COME_SCALARLY" -> let
      fSubj = fValue P.Arg1 frame
      fOrder = fValue P.Order frame
      anchorIssues = if Just True == fmap isAnimate (fOrder >>= fValue P.Anchor) then ["come_scalarly with animate anchor"] else []
      subjIssues = case fSubj of
        Just subj | Nothing == sDeclaredValue P.Type subj -> ["unknown subj"]
        _ -> []
      in anchorIssues ++ subjIssues
    _ -> []
  in {-traceIt "issues" $ -}factIssues ++ (if hasCP then [] else ["no clause"])

orderingIssues frame declaredType = case declaredType of
  "COME_SCALARLY" |
    Just subj <- fValue P.Arg1 frame,
    isJust (sDeclaredValue P.Type subj),
    Just order <- fValue P.Order frame,
    typeEarlier order subj && typeEarlier frame order ->
      ["come_scalarly order subj"]
  "COME_SCALARLY" |
    Just order <- fValue P.Order frame,
    Just relTime <- fValue P.RelTime frame,
    typeEarlier order relTime && typeEarlier relTime frame ->
      ["order relTime COME_SCALARLY"]
  "GO" |
    Just source <- fValue P.Source frame,
    typeEarlier source frame ->
      ["source before GO"]
  _ -> []
