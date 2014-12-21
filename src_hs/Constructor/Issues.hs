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
    in case value fact of
      StrValue attr val -> case (attr, val) of
        (P.Isolation, _) -> \sense ->
          if (isNothing (sValue P.LeftIsolated $ frame sense) || isNothing (sValue P.RightIsolated $ frame sense))
          then issue $ "Incomplete isolation " ++ show (frame sense)
          else finalNo
        (P.Type, declaredType) -> let
          p1 = typeIssues (variable fact) declaredType
          p2 = orderingIssues (variable fact) declaredType
          in \sense -> let
            (IssueOutcome i1 s1, IssueOutcome i2 s2) = (p1 sense, p2 sense)
            in IssueOutcome (i1 ++ i2) (if s1 == Provisional || s2 == Provisional then Provisional else Final)
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

typeIssues var declaredType = let
  frame sense = toFrame sense var
  in case declaredType of
    "seq" -> \sense ->
      if Nothing == sValue P.Conj (frame sense) then issue "comma-only seq" else finalNo
    s | (s == "SIT" || s == "SAY" || s == "FORGET") -> \sense ->
      if isNothing (fValue P.Arg1 (frame sense) >>= sDeclaredValue P.Type) then issue ("unknown " ++ s ++ " subj") else finalNo
    "CASHIER" -> \sense ->
      if any (hasType "OTHERS") (flatten $ fValue P.Place $ frame sense) then issue "cashier of other people" else provNo
    "OPINION" -> \sense ->
      if isNothing (fValue P.Arg1 (frame sense) >>= getType) then issue "opinion without subj" else provNo
    "WORDS" -> \sense ->
      if isNothing (fValue P.Author (frame sense) >>= getType) then issue "words without author" else provNo
    s | (s == "FORGET" || s == "THINK") -> \sense ->
      if isNothing (fValue P.Arg2 (frame sense) >>= getType) then issue (s ++ " without arg2") else provNo
    s | (s == "GO" || s == "CAN" || s == "REMEMBER" || s == "KNOW" || s == "copula_talking_about") -> \sense ->
      if not (and $ map isAnimate $ flatten $ fValue P.Arg1 $ frame sense) then issue ("inanimate " ++ s ++ " subject") else provNo
    "copula_about" -> \sense ->
      if or $ map isAnimate $ flatten $ fValue P.Arg1 (frame sense) then issue ("animate " ++ declaredType ++ " subject") else provNo
    "wh" -> \sense ->
      if isNothing (usage P.Questioned $ frame sense) then issue "non-questioned wh" else finalNo
    "GO" -> \sense ->
      if Just True == fmap isInanimate (fValue P.RelTime (frame sense) >>= fValue P.Anchor) then issue "inanimate GO relTime anchor" else provNo
    "WEATHER_BE" -> \sense ->
      if Just True /= fmap (hasAnyType ["SNOW", "RAIN"]) (fValue P.Arg1 $ frame sense) then issue "non-weather weather_be" else provNo
    "COME_SCALARLY" -> \sense -> let
      fSubj = fValue P.Arg1 $ frame sense
      fOrder = fValue P.Order $ frame sense
      anchorIssues = if Just True == fmap isAnimate (fOrder >>= fValue P.Anchor) then ["come_scalarly with animate anchor"] else []
      subjIssues = case fSubj of
        Just subj | Nothing == sDeclaredValue P.Type subj -> ["unknown subj"]
        _ -> []
      in IssueOutcome (anchorIssues ++ subjIssues) Provisional
    _ -> \sense -> finalNo

orderingIssues var declaredType = let
 frame sense = toFrame sense var
 in case declaredType of
  "COME_SCALARLY" -> \sense -> let
    orderSubj =
      case (fValue P.Arg1 $ frame sense, fValue P.Order $ frame sense) of
        (Just subj, Just order) | isJust (sDeclaredValue P.Type subj) && typeEarlier order subj && typeEarlier (frame sense) order ->
          ["come_scalarly order subj"]
        _ -> []
    orderRelTime =
      case (fValue P.Order $ frame sense, fValue P.RelTime $ frame sense) of
        (Just order, Just relTime) | typeEarlier order relTime && typeEarlier relTime (frame sense) -> ["order relTime COME_SCALARLY"]
        _ -> []
    in IssueOutcome (orderSubj ++ orderRelTime) Provisional
  "GO" -> \sense -> case fValue P.Source $ frame sense of
    Just source | typeEarlier source $ frame sense -> issue "source before GO"
    _ -> provNo
  _ -> \sense -> finalNo

type IssueProvider = Sense -> IssueOutcome
data IssueOutcome = IssueOutcome [Issue] Stability deriving (Show)
data Stability = Final | Provisional deriving (Show,Eq)

issue s = IssueOutcome [s] Provisional
finalNo = IssueOutcome [] Final
provNo = IssueOutcome [] Provisional

data IssueHolder = IssueHolder { finalIssues :: [Issue], provisionalIssues :: [Issue], providers :: [IssueProvider] }

leafHolder sense = makeHolder [] sense (map factIssues $ bareFacts sense)
composeHolders sense holders = makeHolder (holders >>= finalIssues) sense (holders >>= providers)

makeHolder prevFinals sense providers = IssueHolder newFinals (concat newProvisional) newProviders where
  outcomes = [(f, f sense) | f <- providers]
  newFinals = concat [issues | (_, IssueOutcome issues Final) <- outcomes] ++ prevFinals
  (newProvisional, newProviders) = unzip [(issues, f) | (f, IssueOutcome issues Provisional) <- outcomes]

holderIssues holder = {-traceIt "issues" $ -}provisionalIssues holder ++ finalIssues holder
