module Constructor.EnglishAdjectives(adjectiveFrames, adjectiveString, adjectiveArgs, specialAdjectives, isElidedNoun, shouldContrastByGender) where
import Constructor.Sense
import Constructor.Inference
import Constructor.ArgumentPlanning
import Constructor.Util
import qualified Constructor.SemanticProperties as P

import Data.Maybe

adjectiveFrames :: Frame -> [Frame]
adjectiveFrames nounFrame = catMaybes $ map (\attr -> fValue attr nounFrame) [P.Order, P.Property, P.State, P.Kind, P.Size, P.Quality, P.Color]

adjectiveString nounFrame adjFrame = case getType adjFrame of
  Nothing -> ""
  Just s -> case s of
    "3" -> "third"
    "4" -> "fourth"
    "5" -> "fifth"
    "6" -> "sixth"
    "AMAZING" -> "amazing"
    "BIG" -> if hasType "GARDEN" nounFrame then "big" else "great"
    "BLIND" -> "blind"
    "BOILED" -> "boiled"
    "CLEVER" -> "smart"
    "CLOSED" -> "closed"
    "COMMERCIAL" -> "commercial"
    "EXCESSIVE" -> "excessive"
    "FALL_OUT" -> "falling"
    "FAST" -> "fast"
    "GREEN" -> "green"
    "HUMBLE" -> "humble"
    "LITTLE" -> "small"
    "MORE" | Just theme <- fValue P.Theme adjFrame -> case getType theme of
      Just "CLEVER" -> "smarter"
      Just "FAST" -> "faster"
      Just "BIG" -> "larger"
      Just "GOOD" -> "better"
      _ -> "more" `cat` adjectiveString nounFrame theme
    "ROASTED" -> "roasted"
    "SIMILAR_TO" -> "like"
    "SMASHED" -> if not (isElidedNoun nounFrame) then "smashed" else ""
    "STUPID" -> "stupid"
    "UNEMBRACEABLE" -> "unembraceable"
    "UNILATERAL" -> "unilateral"
    "WOVEN" -> "woven"
    "RED" -> "red"
    _ -> s

adjectiveArgs valFrame = case getType valFrame of
  Just "SIMILAR_TO" | Just arg2 <- fValue P.Arg2 valFrame -> [NPArg arg2]
  Just "MORE" | Just a <- fValue P.Anchor valFrame -> [PPArg "than" a]
  _ -> []

specialAdjectives nounFrame = gender `cat` shopKind where
  shopKind = if sValue P.Name nounFrame == Just "гастроном" then "grocery" else ""
  gender =
        if shouldContrastByGender nounFrame && isHuman nounFrame
        then case sValue P.RusGender nounFrame of
          Just "Masc" -> "male"
          Just "Fem" -> "female"
          _ -> ""
        else ""

isElidedNoun frame = Just "true" == sValue P.ElidedNoun frame

shouldContrastByGender frame = case (getType frame, sValue P.RusGender frame) of
  (Just typ, Just gender) | typ /= "NAMED" -> let
    differentGender candidate = case sValue P.RusGender candidate of
      Just g1 -> g1 /= gender
      _ -> False
    contrastible candidate =
      if not (isNumber $ Just candidate) && hasType typ candidate && differentGender candidate then
        case usage P.Arg1 candidate of
          Just fVerb -> not (isVerbEllipsis fVerb) || isEllipsisAnchor (Just candidate) fVerb
          _ -> True
      else False
    in any contrastible $ allFrames $ sense frame
  _ -> False
