module Constructor.EnglishNouns (noun, isSingular, renderAsWord) where

import Constructor.Sense
import Constructor.Inference
import Data.Maybe
import Constructor.Util
import qualified Constructor.SemanticProperties as P
import Data.Char (toLower)

noun Nothing _ = "??"
noun (Just typ) frame = case typ of
  "AMAZE" -> "amazement"
  "ARGUE" -> "argument"
  "CARROT" -> "carrots"
  "CASE" -> "thing"
  "COUNTING" -> if isJust (usage P.Domain frame) then "count" else "counting"
  "CUNNING_PERSON" -> "cunning person"
  "FINGER" -> if isSingular frame then "finger" else "fingers"
  "GARDEN" -> if (fValue P.VName frame >>= sValue P.Name) == Just "летний" then "Summer Garden" else "garden"
  "JAW" -> if isSingular frame then "jaw" else "jaws"
  "MARKET" -> "Maltsev market"
  "NAMED" -> fromMaybe "??name" $ sValue P.Name frame
  "OLD_LADY" -> "old lady"
  "OLD_LADIES" -> "old ladies"
  "OLD_MAN" -> "old man"
  "SALESPERSON" -> case usage P.Arg2 (unSeq frame) of
    Just copula | hasType "copula" copula, Just "Fem" == (fValue P.Arg1 copula >>= sValue P.RusGender) -> "saleswoman"
    _ -> "salesman"
  "SHOP" -> "store"
  "SMASHED_ONE" -> "one who was smashed"
  "THIS" -> if Just "DISTRACT" == (usage P.Arg1 frame >>= getType) then "that" else "this"
  _ ->
    if isNumberString typ && renderAsWord frame then case typ of
      "1" -> "one"
      "2" -> "two"
      "3" -> "three"
      "4" -> "four"
      "5" -> "five"
      "6" -> "six"
      "7" -> "seven"
      "8" -> "eight"
      "9" -> "nine"
      "10" -> "ten"
      _ -> typ
    else map toLower typ

renderAsWord frame = not $ isNumber $ Just frame

isSingular frame = case getType frame of
  Just "NEIGHBORS" -> False
  Just "WINDOWS" -> False
  Just "OLD_LADIES" -> False
  Just "THEY" -> False
  Just "WE" -> False
  Just "TREES" -> False
  _ -> case fValue P.Quantifier frame >>= getType of
    Just s -> s == "1"
    _ -> case fValue P.Specifier_all frame >>= getType of
      Just "ALL" -> False
      _ -> True
