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

l x = [x]

factIssues :: Fact -> [IssueProvider]
factIssues fact = let
    frame sense = toFrame sense (variable fact)
    in case value fact of
      StrValue attr val -> case (attr, val) of
        (P.Isolation, _) -> l $ \sense ->
          if (isNothing (sDeclaredValue P.LeftIsolated $ frame sense) || isNothing (sDeclaredValue P.RightIsolated $ frame sense))
          then issue $ "Incomplete isolation " ++ show (frame sense)
          else finalNo
        (P.Type, declaredType) -> typeIssues (variable fact) declaredType ++ orderingIssues (variable fact) declaredType
        _ -> l $ \sense -> finalNo
      VarValue attr val -> let
        valFrame sense = toFrame sense val
        in case attr of
          P.AccordingTo -> l $ \sense ->
            if any (not . hasAnyType ["WORDS", "OPINION"]) (flatten $ Just $ valFrame sense) then issue "invalid accordingTo" else provNo
          P.OptativeModality -> l $ \sense ->
            if any (not . hasAnyType ["LUCK"]) (flatten $ Just $ valFrame sense) then issue "invalid optativeModality" else provNo
          P.Quantifier -> l $ \sense ->
            if hasType "EYES" (frame sense) && not (hasType "2" $ valFrame sense) then issue "suspicious eye count" else provNo
          P.Condition -> l $ \sense ->
            if not (hasType "CASE" $ valFrame sense) then issue "wrong condition" else provNo
          P.Relative -> l $ \sense ->
            case fValue P.Questioned $ valFrame sense of
              Just wh ->
                if hasAnyType ["WE", "THEY"] $ frame sense then issue "relative clause for pronoun/number"
                else if Just wh == (fValue P.Content (valFrame sense) >>= fValue P.VTime) then issue "time relative clause"
                else provNo
              Nothing -> provNo
          P.Topic -> l $ \sense ->
            if hasType "ASK" (frame sense) && any isFactCP (flatten $ Just $ valFrame sense) then issue "asking fact" else provNo
          _ | attr `elem` [P.Location, P.Location_on, P.Location_in] -> l $ \sense ->
            if hasAnyType ["CASE", "COUNTING"] (valFrame sense) then issue "wrong location" else provNo
          _ -> l $ \sense -> finalNo

isTypeDefined frame = isJust frame && all (\f -> isJust (getDeclaredType f)) (flatten frame)

requireType mFrame f = case mFrame of
  Just frame | isTypeDefined mFrame -> f frame
  _ -> provNo

typeIssues var declaredType = let
  frame sense = toFrame sense var
  in case declaredType of
    "seq" -> l $ \sense ->
      if Nothing == sDeclaredValue P.Conj (frame sense) then issue "comma-only seq" else finalNo
    s | (s == "SIT" || s == "SAY" || s == "FORGET") -> l $ \sense ->
      if isNothing (fValue P.Arg1 (frame sense) >>= sDeclaredValue P.Type) then issue ("unknown " ++ s ++ " subj") else finalNo
    "CASHIER" -> l $ \sense ->
      if any (hasType "OTHERS") (flatten $ fValue P.Place $ frame sense) then issue "cashier of other people" else provNo
    "OPINION" -> l $ \sense ->
      if isNothing (fValue P.Arg1 (frame sense) >>= getType) then issue "opinion without subj" else finalNo
    "WORDS" -> l $ \sense ->
      if isNothing (fValue P.Author (frame sense) >>= getType) then issue "words without author" else finalNo
    s | (s == "FORGET" || s == "THINK") -> l $ \sense ->
      if isNothing (fValue P.Arg2 (frame sense) >>= getType) then issue (s ++ " without arg2") else finalNo
    s | (s == "GO" || s == "CAN" || s == "REMEMBER" || s == "KNOW" || s == "copula_talking_about") -> l $ \sense ->
      requireType (fValue P.Arg1 $ frame sense) $ \fSubj ->
        if not (and $ map isAnimate $ flatten $ Just fSubj) then finalIssue ("inanimate " ++ s ++ " subject") else finalNo
    "copula_about" -> l $ \sense ->
      requireType (fValue P.Arg1 $ frame sense) $ \fSubj ->
        if or $ map isAnimate $ flatten $ Just fSubj then finalIssue ("animate " ++ declaredType ++ " subject") else finalNo
    "wh" -> l $ \sense ->
      if isNothing (usage P.Questioned $ frame sense) then issue "non-questioned wh" else finalNo
    "GO" -> l $ \sense ->
      if Just True == fmap isInanimate (fValue P.RelTime (frame sense) >>= fValue P.Anchor) then issue "inanimate GO relTime anchor" else provNo
    "WEATHER_BE" -> l $ \sense ->
      if Just True /= fmap (hasAnyType ["SNOW", "RAIN"]) (fValue P.Arg1 $ frame sense) then issue "non-weather weather_be" else provNo
    "COME_SCALARLY" -> let
      anchorIssues = \sense -> requireType (fValue P.Order (frame sense) >>= fValue P.Anchor) $ \anchor ->
        if isAnimate anchor then finalIssue "come_scalarly with animate anchor" else finalNo
      subjIssues = \sense -> case fValue P.Arg1 $ frame sense of
        Just subj -> if Nothing == sDeclaredValue P.Type subj then issue "unknown subj" else finalNo
        _ -> provNo
      in [anchorIssues, subjIssues]
    _ -> l $ \sense -> finalNo

orderingIssues var declaredType = let
 frame sense = toFrame sense var
 in case declaredType of
  "COME_SCALARLY" -> let
    orderSubj = \sense ->
      requireType (fValue P.Arg1 $ frame sense) $ \subj ->
        requireType (fValue P.Order $ frame sense) $ \order ->
          if typeEarlier order subj && typeEarlier (frame sense) order then finalIssue "come_scalarly order subj" else finalNo
    orderRelTime = \sense ->
      requireType (fValue P.Order $ frame sense) $ \order ->
        requireType (fValue P.RelTime $ frame sense) $ \relTime ->
          if typeEarlier order relTime && typeEarlier relTime (frame sense) then finalIssue "order relTime COME_SCALARLY" else finalNo
    in [orderSubj, orderRelTime]
  "GO" -> l $ \sense ->
    requireType (fValue P.Source $ frame sense) $ \source ->
      if typeEarlier source $ frame sense then finalIssue "source before GO" else finalNo
  _ -> l $ \sense -> finalNo

type IssueProvider = Sense -> IssueOutcome
data IssueOutcome = IssueOutcome [Issue] Stability deriving (Show)
data Stability = Final | Provisional deriving (Show,Eq)

issue s = IssueOutcome [s] Provisional
finalIssue s = IssueOutcome [s] Final
finalNo = IssueOutcome [] Final
provNo = IssueOutcome [] Provisional

data IssueHolder = IssueHolder { finalIssues :: [Issue], provisionalIssues :: [Issue], providers :: [IssueProvider] }

leafHolder sense = makeHolder [] sense (bareFacts sense >>= factIssues)
composeHolders sense holders = makeHolder (holders >>= finalIssues) sense (holders >>= providers)

makeHolder prevFinals sense providers = IssueHolder newFinals (concat newProvisional) newProviders where
  outcomes = [(f, f sense) | f <- providers]
  newFinals = concat [issues | (_, IssueOutcome issues Final) <- outcomes] ++ prevFinals
  (newProvisional, newProviders) = unzip [(issues, f) | (f, IssueOutcome issues Provisional) <- outcomes]

holderIssues holder = {-traceIt "issues" $ -}provisionalIssues holder ++ finalIssues holder
