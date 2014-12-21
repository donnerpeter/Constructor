module Constructor.Issues (IssueHolder, leafHolder, composeHolders, holderIssues) where

import Constructor.Mite
import Constructor.Sense
import Constructor.Inference
import Constructor.Util
import Constructor.Variable
import Control.Monad
import Data.Maybe
import qualified Constructor.SemanticProperties as P

type Issue = String

factIssues :: Fact -> IssueProvider
factIssues fact = let
    frame sense = toFrame sense (variable fact)
    issue s = IssueOutcome [s] Provisional
    finalNo = IssueOutcome [] Final
    provNo = IssueOutcome [] Provisional
    in case value fact of
      StrValue attr val -> case (attr, val) of
        (P.Isolation, _) -> \sense ->
          if (isNothing (sValue P.LeftIsolated $ frame sense) || isNothing (sValue P.RightIsolated $ frame sense))
          then issue $ "Incomplete isolation " ++ show (frame sense)
          else finalNo
        (P.Type, declaredType) -> \sense ->
          IssueOutcome (typeIssues (frame sense) declaredType ++ orderingIssues (frame sense) declaredType) Provisional
        _ -> \sense -> finalNo
      VarValue attr val -> let
        valFrame sense = toFrame sense val
        in case attr of
          P.AccordingTo -> \sense ->
            if any (not . hasAnyType ["WORDS", "OPINION"]) (flatten $ Just $ valFrame sense) then issue "invalid accordingTo" else provNo
          P.OptativeModality -> \sense ->
            if any (not . hasAnyType ["LUCK"]) (flatten $ Just $ valFrame sense) then issue "invalid optativeModality" else provNo
          P.Quantifier -> \sense ->
            if hasType "EYES" (frame sense) && not (hasType "2" $ valFrame sense) then issue "suspicious eye count" else provNo
          P.Condition -> \sense ->
            if not (hasType "CASE" $ valFrame sense) then issue "wrong condition" else provNo
          P.Relative -> \sense ->
            case fValue P.Questioned $ valFrame sense of
              Just wh ->
                if hasAnyType ["WE", "THEY"] $ frame sense then issue "relative clause for pronoun/number"
                else if Just wh == (fValue P.Content (valFrame sense) >>= fValue P.VTime) then issue "time relative clause"
                else provNo
              Nothing -> provNo
          P.Topic -> \sense ->
            if hasType "ASK" (frame sense) && any isFactCP (flatten $ Just $ valFrame sense) then issue "asking fact" else provNo
          _ | attr `elem` [P.Location, P.Location_on, P.Location_in] -> \sense ->
            if hasAnyType ["CASE", "COUNTING"] (valFrame sense) then issue "wrong location" else provNo
          _ -> \sense -> finalNo

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
    "wh" | isNothing (usage P.Questioned frame) -> ["non-questioned wh"]
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

type IssueProvider = Sense -> IssueOutcome
data IssueOutcome = IssueOutcome [Issue] Stability deriving (Show)
data Stability = Final | Provisional deriving (Show)

data IssueHolder = IssueHolder { finalIssues :: [Issue], provisionalIssues :: [Issue], providers :: [IssueProvider] }

leafHolder sense = makeHolder [] sense (map factIssues $ bareFacts sense)
composeHolders sense holders = makeHolder (holders >>= finalIssues) sense (holders >>= providers)

makeHolder prevFinals sense providers = IssueHolder newFinals (concat newProvisional) newProviders where
  outcomes = [(f, f sense) | f <- providers]
  newFinals = concat [issues | (_, IssueOutcome issues Final) <- outcomes] ++ prevFinals
  (newProvisional, newProviders) = unzip [(issues, f) | (f, IssueOutcome issues Provisional) <- outcomes]

holderIssues holder = {-traceIt "issues" $ -}provisionalIssues holder ++ finalIssues holder
