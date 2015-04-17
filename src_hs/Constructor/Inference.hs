module Constructor.Inference (
  findFrames,
  fValue, sValue,
  getType, getDeclaredType, hasType, hasAnyType, resolve,
  typeEarlier,
  flatten, isNumber, unSeq, unSeq1, unSeq2, seqSiblings, prevSiblings, nextSiblings,
  isHuman, isAnimate, isInanimate,
  isCP, isFactCP, isQuestionCP, isQualityCopula, isOwnerCopula) where

import Constructor.Sense
import qualified Constructor.SemanticProperties as P
import Data.Maybe
import Control.Monad
import Control.Applicative
import Data.List (find)
import Constructor.Util

findFrames typ sense = [f | f <- allFrames sense, hasType typ f]

typeEarlier f1 f2 = earlier f1 (typeAttr f1) f2 (typeAttr f2) where
  typeAttr f = if isJust (getDeclaredType f) then P.Type else P.Elided

sValue attr frame =
  let declared = sDeclaredValue attr frame
      original = sValue attr =<< fDeclaredValue P.EllipsisOriginal frame
  in
  if isJust declared then declared
  else if isJust original then original
  else
    case attr of
      P.Given ->
        if Just True == fmap (hasAnyType ["SOME", "ONE"]) (fValue P.Determiner frame) then Just "false"
        else if hasAnyType ["CASE", "HAMMER", "TREES", "BENCH", "FINGER", "JAW", "SHAWL", "SPECIALIST", "GUMBOIL"] frame then Just "false"
        else if isTrue $ isNumberString <$> getType frame then Just "false"
        else if Just "copula" == (usage P.Arg2 (unSeq frame) >>= getDeclaredType) then Just "false"
        else if isTrue $ isQualityCopula <$> usage P.Arg1 frame then Just "false"
        else if isTrue $ isOwnerCopula <$> usage P.Arg1 frame then Just "false"
        else if hasType "CASHIER" frame then
          case find (\cashier -> typeEarlier cashier frame) $ findFrames "CASHIER" $ sense frame of
             Just _ -> Just "true"
             _ -> case find (\shop -> typeEarlier shop frame) $ findFrames "SHOP" $ sense frame of
               Just shop -> sValue P.Given shop
               _ -> Just "true"
        else if hasType "SHOP" frame then
          if any (\cashier -> typeEarlier cashier frame && Just "copula" /= (usage P.Arg2 cashier >>= getType)) $ findFrames "CASHIER" $ sense frame then Just "true"
          else if isJust $ msum [usage P.Arg1 frame, usage P.Source frame] then Just "true"
          else Just "false"
        else if isTrue $ isCP <$> usage P.Content frame then Just "false"
        else Just "true"
      P.Type -> case usage P.Arg1 frame >>= commandingSubject >>= getType of
        Just commandingType -> Just commandingType
        Nothing ->
          case (sDeclaredValue P.RusNumber frame, sDeclaredValue P.RusPerson frame, sDeclaredValue P.RusGender frame) of
            (Just "Pl", Just "3", _) -> Just "THEY"
            (Just "Sg", Just "3", Just "Fem") -> Just "SHE"
            (Just "Sg", Just "3", _) -> Just "HE"
            _ -> Nothing
      P.RusNumber -> case sDeclaredValue P.Type frame of
        Just "WE" -> Just "Pl"
        Just "THEY" -> Just "Pl"
        Just "ME" -> Just "Sg"
        _ -> Nothing
      P.ExclamativeQuestion | isQuestionCP frame && isNothing (sDeclaredValue P.Question_mark frame) && null (usageFacts frame) ->
        Just "true"
      _ -> Nothing


fValue attr frame =
  let declared = fDeclaredValue attr frame
      original = fValue attr =<< fDeclaredValue P.EllipsisOriginal frame
  in
  if isJust declared then declared
  else if isJust original then original
  else case attr of
      P.Arg1 ->
        if hasAnyType ["MOUTH", "NOSE", "JAW", "JAWS", "FINGER", "NEIGHBORS", "CURIOSITY"] frame then let
          verbs = catMaybes [usage P.Source $ unVariants $ unSeq frame,
                             usage P.Arg2 $ unVariants $ unSeq frame,
                             usage P.Instrument $ unVariants $ unSeq frame,
                             usage P.Reason $ unVariants $ unSeq frame,
                             usage P.Goal_to $ unVariants $ unSeq frame]
          foregrounds = catMaybes $ map (usage P.PerfectBackground) verbs
          unVariants frame = unSeq $ fromMaybe frame (usage P.Variants frame)
          in fmap resolve $ msum $ map (fValue P.Receiver) (verbs ++ foregrounds) ++ map (fValue P.Arg1) (verbs ++ foregrounds)
        else Nothing
      P.AccordingTo | Just caze <- usage P.Content frame >>= usage P.WhenCondition, hasType "CASE" caze -> fValue P.AccordingTo caze
      P.Source | Just typ <- getType frame -> let
        antecedents = reverse $ filter (flip typeEarlier frame) $ findFrames typ (sense frame)
        in msum $ map (fDeclaredValue P.Source) antecedents
      _ -> Nothing

commandingSubject frame = msum [usage P.Content frame >>= usage P.Theme, usage P.Content frame >>= usage P.Arg2] >>= fValue P.Arg1

hasType t frame = getType frame == Just t
hasAnyType types frame = fromMaybe False $ getType frame >>= \t -> Just $ elem t types
getType frame = sValue P.Type frame
getDeclaredType frame = sDeclaredValue P.Type frame

flatten Nothing = []
flatten (Just frame) = if Just "seq" == getDeclaredType frame then flatten (fValue P.Member1 frame) ++ maybeToList (fValue P.Member2 frame) else [frame]

seqSiblings frame = flatten $ Just $ unSeq frame
prevSiblings frame = takeWhile (/= frame) $ seqSiblings frame
nextSiblings frame = tail $ dropWhile (/= frame) $ seqSiblings frame

resolve frame = case getType frame of
  Just "SELF" | Just target <- fValue P.Target frame -> target
  Just "wh" | Just relativized <- usage P.Questioned frame >>= usage P.Relative -> relativized
  Just "HE" -> let
    copulas = findFrames "copula" $ sense frame
    arg2s = reverse $ filter (\f -> not (isFrameReachable f frame) && typeEarlier f frame && not (hasAnyType ["wh", "placeholder"] f)) $ catMaybes $ map (fValue P.Arg2) copulas
    in fromMaybe frame $ listToMaybe arg2s
  _ -> frame

isNumber frame = any (\f -> sValue P.Number f == Just "true") $ flatten frame

isHuman frame = hasAnyType ["NEIGHBOR", "NEIGHBORS", "CASHIER", "NAMED", "EVERYBODY",
  "ME", "HE", "SHE", "THEY" -- todo pronouns are not necessarily animate
  ] frame
isAnimate frame = isHuman frame || Just "true" == sValue P.Animate frame

isInanimate frame = Just "wh" == getDeclaredType frame && Just "true" /= sValue P.Animate frame
                 || Just True == fmap isNumberString (getType frame)

unSeq frame = inner True frame where
  inner allow2 frame = case (usage P.Member1 frame, usage P.Member2 frame) of
    (Just s, _) -> inner allow2 s
    (_, Just s) | allow2 -> inner False s
    _ -> frame

unSeq1 frame = case usage P.Member1 frame of
  Just s -> unSeq1 s
  _ -> frame

unSeq2 frame = case usage P.Member2 frame of
  Just s -> unSeq2 s
  _ -> frame

isCP frame = hasType "situation" frame

isFactCP frame = isCP frame && not (isQuestionCP frame)
isQuestionCP frame = isCP frame && isJust (fValue P.Questioned frame)

isQualityCopula frame = hasType "copula" frame && isTrue (hasType "placeholder" <$> arg2) && isTrue (hasType "wh" <$> (fValue P.Quality =<< arg2)) where
  arg2 = fValue P.Arg2 frame

isOwnerCopula fVerb = hasType "copula" fVerb && isJust (fValue P.Owner =<< fValue P.Arg2 fVerb)
