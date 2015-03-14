module Constructor.Issues (IssueHolder, leafHolder, composeHolders, holderIssues, fatalIssues, holderSense) where

import Constructor.Sense
import Constructor.Inference
import Constructor.Util
import Constructor.Variable
import Control.Monad
import Data.Maybe
import qualified Constructor.SemanticProperties as P

type Issue = (String, Fatality)

l x = [x]

factIssues :: Fact -> [IssueProvider]
factIssues fact = let
    frame sense = toFrame sense (variable fact)
    in case value fact of
      StrValue attr val -> case (attr, val) of
        (P.Isolation, _) -> l $ \sense -> let
          f = frame sense
          isolated = catMaybes [sDeclaredValue P.LeftIsolated f, sDeclaredValue P.RightIsolated f]
          msg = "Incomplete isolation " ++ show f
          in
          if "false" `elem` isolated then finalIssue msg
          else if null isolated then finalNo
          else issue msg
        (P.ElidedNoun, _) -> l $ \_ -> finalIssue "elided"
        (P.Type, declaredType) -> typeIssues (variable fact) declaredType ++ orderingIssues (variable fact) declaredType
        _ -> []
      VarValue attr val -> let
        valFrame sense = toFrame sense val
        requireValType f = l$ \sense -> requireType (Just $ valFrame sense) f
        in case attr of
          P.AccordingTo -> requireValType $ \valFrame ->
            if any (not . hasAnyType ["WORDS", "OPINION"]) (flatten $ Just valFrame) then fatalIssue "invalid accordingTo" else finalNo
          P.OptativeModality -> requireValType $ \valFrame ->
            if any (not . hasAnyType ["LUCK", "BY_THE_WAY"]) (flatten $ Just valFrame) then fatalIssue "invalid optativeModality" else finalNo
          P.Quantifier -> l $ \sense ->
            requireType (Just $ frame sense) $ \f ->
              requireType (Just $ valFrame sense) $ \valFrame ->
                if hasType "EYES" (frame sense) && not (hasType "2" valFrame) then finalIssue "suspicious eye count" else finalNo
          P.Condition -> requireValType $ \valFrame ->
            if not (hasType "CASE" valFrame) then fatalIssue "wrong condition" else finalNo
          P.Mood -> requireValType $ \valFrame ->
            if not (hasAnyType ["RELIEF", "JOY"] valFrame) then fatalIssue "wrong mood" else finalNo
          P.Companion -> requireValType $ \valFrame ->
            if not (isHuman valFrame) then fatalIssue "wrong companion" else finalNo
          P.Relative -> l $ \sense ->
            requireType (Just $ frame sense) $ \frame ->
              requireType (fValue P.Questioned $ valFrame sense) $ \wh ->
                if hasAnyType ["WE", "THEY"] frame then finalIssue "relative clause for pronoun"
                else if Just wh == (fValue P.Content (valFrame sense) >>= fValue P.VTime) then fatalIssue "time relative clause"
                else finalNo
          P.Topic -> l $ \sense ->
            requireType (Just $ frame sense) $ \frame ->
              requireType (Just $ valFrame sense) $ \valFrame ->
                if hasType "ASK" frame && any isFactCP (flatten $ Just valFrame) then fatalIssue "asking fact" else finalNo
          _ | attr `elem` [P.Location, P.Location_on, P.Location_in] -> requireValType $ \valFrame ->
            if hasAnyType ["CASE", "COUNTING"] valFrame then fatalIssue "wrong location" else finalNo
          _ -> []

isTypeDefined frame = isJust frame && all (\f -> isJust (getDeclaredType f)) (flatten frame)

requireType mFrame f = case mFrame of
  Just frame | isTypeDefined mFrame -> f frame
  _ -> provNo

typeIssues var declaredType = missingSubj ++ missingArg2 ++ inanimateSubj ++ adjCopula ++ other where
  frame sense = toFrame sense var
  missingSubj =
    if declaredType `elem` ["SIT", "SAY", "FORGET", "COME_SCALARLY", "copula"] then l $ \sense ->
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
  adjCopula =
    if declaredType == "copula" then l $ \sense ->
      requireType (fValue P.Arg1 $ frame sense) $ \fSubj ->
        requireType (fValue P.Arg2 $ frame sense) $ \fObj ->
          if hasType "placeholder" fObj && usage P.Arg1 fSubj /= Just (frame sense) then finalIssue "adj copula in a conjunction"
          else provNo
    else []
  other = case declaredType of
    "seq" -> l $ \sense ->
      if Nothing == sDeclaredValue P.Conj (frame sense) then issue "comma-only seq" else finalNo
    "CASHIER" -> l $ \sense ->
      requireType (fValue P.Place $ frame sense) $ \fPlace ->
        if any (hasType "OTHERS") (flatten $ Just fPlace) then fatalIssue "cashier of other people" else finalNo
    "OPINION" -> l $ \sense ->
      if isNothing (fValue P.Arg1 (frame sense) >>= getType) then issue "opinion without subj" else finalNo
    "WORDS" -> l $ \sense ->
      if isNothing (fValue P.Author (frame sense) >>= getType) then issue "words without author" else finalNo
    "copula_about" -> l $ \sense ->
      requireType (fValue P.Arg1 $ frame sense) $ \fSubj ->
        if or $ map isAnimate $ flatten $ Just fSubj then fatalIssue ("animate " ++ declaredType ++ " subject") else finalNo
    "wh" -> l $ \sense ->
      if isNothing (usage P.Questioned $ frame sense) then issue "non-questioned wh" else finalNo
    "GO" -> l $ \sense ->
      requireType (fValue P.RelTime (frame sense) >>= fValue P.Anchor) $ \fAnchor ->
        if isInanimate fAnchor then finalIssue "inanimate GO relTime anchor" else finalNo
    "WEATHER_BE" -> l $ \sense ->
      requireType (fValue P.Arg1 (frame sense)) $ \fSubj ->
        if not $ hasAnyType ["SNOW", "RAIN"] fSubj then fatalIssue "non-weather weather_be" else finalNo
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
data Fatality = Fatal | Bearable deriving (Show,Eq)

issue s = IssueOutcome [(s, Bearable)] Provisional
finalIssue s = IssueOutcome [(s, Bearable)] Final
fatalIssue s = IssueOutcome [(s, Fatal)] Final
finalNo = IssueOutcome [] Final
provNo = IssueOutcome [] Provisional

data IssueHolder = IssueHolder { finalIssues :: [Issue], provisionalIssues :: [Issue], providers :: [IssueProvider], holderSense :: Sense }
instance Show IssueHolder where show ih = show $ holderIssues ih

leafHolder sense = makeHolder [] sense (bareFacts sense >>= factIssues)
composeHolders holders = makeHolder (holders >>= finalIssues) (composeSense $ map holderSense holders) (holders >>= providers)

makeHolder prevFinals sense providers = IssueHolder newFinals (concat newProvisional) newProviders sense where
  outcomes = [(f, f sense) | f <- providers]
  newFinals = concat [issues | (_, IssueOutcome issues Final) <- outcomes] ++ prevFinals
  (newProvisional, newProviders) = unzip [(issues, f) | (f, IssueOutcome issues Provisional) <- outcomes]

holderIssues holder = {-traceIt "issues" $ -}provisionalIssues holder ++ finalIssues holder

fatalIssues holder = [s | (s, Fatal) <- finalIssues holder]