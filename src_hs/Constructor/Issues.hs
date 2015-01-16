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
        _ -> []
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
            requireType (Just $ frame sense) $ \frame ->
              requireType (fValue P.Questioned $ valFrame sense) $ \wh ->
                if hasAnyType ["WE", "THEY"] frame then finalIssue "relative clause for pronoun"
                else if Just wh == (fValue P.Content (valFrame sense) >>= fValue P.VTime) then finalIssue "time relative clause"
                else finalNo
          P.Topic -> l $ \sense ->
            requireType (Just $ frame sense) $ \frame ->
              requireType (Just $ valFrame sense) $ \valFrame ->
                if hasType "ASK" frame && any isFactCP (flatten $ Just valFrame) then finalIssue "asking fact" else finalNo
          _ | attr `elem` [P.Location, P.Location_on, P.Location_in] -> l $ \sense ->
            if hasAnyType ["CASE", "COUNTING"] (valFrame sense) then issue "wrong location" else provNo
          _ -> []

isTypeDefined frame = isJust frame && all (\f -> isJust (getDeclaredType f)) (flatten frame)

requireType mFrame f = case mFrame of
  Just frame | isTypeDefined mFrame -> f frame
  _ -> provNo

typeIssues var declaredType = missingSubj ++ missingArg2 ++ inanimateSubj ++ other where
  frame sense = toFrame sense var
  missingSubj =
    if declaredType `elem` ["SIT", "SAY", "FORGET", "COME_SCALARLY"] then l $ \sense ->
      if isNothing (fValue P.Arg1 (frame sense) >>= sDeclaredValue P.Type) then issue ("unknown " ++ declaredType ++ " subj") else finalNo
    else []
  missingArg2 =
    if declaredType `elem` ["FORGET", "THINK"] then l $ \sense ->
      if isNothing (fValue P.Arg2 (frame sense) >>= getType) then issue (declaredType ++ " without arg2") else finalNo
    else []
  inanimateSubj =
    if declaredType `elem` ["GO", "CAN", "REMEMBER", "KNOW", "copula_talking_about"] then l $ \sense ->
      requireType (fValue P.Arg1 $ frame sense) $ \fSubj ->
        if not (and $ map isAnimate $ flatten $ Just fSubj) then finalIssue ("inanimate " ++ declaredType ++ " subject") else finalNo
    else []
  other = case declaredType of
    "seq" -> l $ \sense ->
      if Nothing == sDeclaredValue P.Conj (frame sense) then issue "comma-only seq" else finalNo
    "CASHIER" -> l $ \sense ->
      if any (hasType "OTHERS") (flatten $ fValue P.Place $ frame sense) then issue "cashier of other people" else provNo
    "OPINION" -> l $ \sense ->
      if isNothing (fValue P.Arg1 (frame sense) >>= getType) then issue "opinion without subj" else finalNo
    "WORDS" -> l $ \sense ->
      if isNothing (fValue P.Author (frame sense) >>= getType) then issue "words without author" else finalNo
    "copula_about" -> l $ \sense ->
      requireType (fValue P.Arg1 $ frame sense) $ \fSubj ->
        if or $ map isAnimate $ flatten $ Just fSubj then finalIssue ("animate " ++ declaredType ++ " subject") else finalNo
    "wh" -> l $ \sense ->
      if isNothing (usage P.Questioned $ frame sense) then issue "non-questioned wh" else finalNo
    "GO" -> l $ \sense ->
      if Just True == fmap isInanimate (fValue P.RelTime (frame sense) >>= fValue P.Anchor) then issue "inanimate GO relTime anchor" else provNo
    "WEATHER_BE" -> l $ \sense ->
      if Just True /= fmap (hasAnyType ["SNOW", "RAIN"]) (fValue P.Arg1 $ frame sense) then issue "non-weather weather_be" else provNo
    "COME_SCALARLY" -> l $ \sense ->
      requireType (fValue P.Order (frame sense) >>= fValue P.Anchor) $ \anchor ->
        if isAnimate anchor then finalIssue "come_scalarly with animate anchor" else finalNo
    _ -> []

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
instance Show IssueHolder where show ih = show $ holderIssues ih

leafHolder sense = makeHolder [] sense (bareFacts sense >>= factIssues)
composeHolders sense holders = makeHolder (holders >>= finalIssues) sense (holders >>= providers)

makeHolder prevFinals sense providers = IssueHolder newFinals (concat newProvisional) newProviders where
  outcomes = [(f, f sense) | f <- providers]
  newFinals = concat [issues | (_, IssueOutcome issues Final) <- outcomes] ++ prevFinals
  (newProvisional, newProviders) = unzip [(issues, f) | (f, IssueOutcome issues Provisional) <- outcomes]

holderIssues holder = {-traceIt "issues" $ -}provisionalIssues holder ++ finalIssues holder
