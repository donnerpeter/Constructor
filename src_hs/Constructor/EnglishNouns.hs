module Constructor.EnglishNouns (noun, isSingular, renderAsWord) where

import Constructor.Sense
import Data.Maybe
import Constructor.Util
import qualified Constructor.SemanticProperties as P

noun Nothing _ = "??"
noun (Just typ) frame = case typ of
  "AMAZE" -> "amazement"
  "APARTMENTS" -> "apartments"
  "ARGUE" -> "argument"
  "BENCH" -> "bench"
  "BOOK" -> "book"
  "CASE" -> "thing"
  "CASHIER" -> "cashier"
  "CHILD" -> "child"
  "CORNER" -> "corner"
  "COUNTING" -> if isJust (usage P.Domain frame) then "count" else "counting"
  "EYES" -> "eyes"
  "FAMILY" -> "family"
  "FINGER" -> if isSingular frame then "finger" else "fingers"
  "GARDEN" -> if (fValue P.VName frame >>= getType) == Just "летний" then "Summer Garden" else "garden"
  "HAMMER" -> "hammer"
  "HE" -> "he"
  "HOUSE" -> "house"
  "JAW" -> if isSingular frame then "jaw" else "jaws"
  "JAWS" -> "jaws"
  "JOY" -> "joy"
  "MATTER" -> "matter"
  "ME" -> "me"
  "MEANING" -> "meaning"
  "MONEY" -> "money"
  "MOUTH" -> "mouth"
  "NAMED_PERSON" -> fromMaybe "??name" $ sValue P.Name frame
  "NEIGHBOR" -> "neighbor"
  "NEIGHBORS" -> "neighbors"
  "NOSE" -> "nose"
  "OFFICES" -> "offices"
  "OPINION" -> "opinion"
  "ORDER" -> "order"
  "OTHERS" -> "others"
  "PREDICAMENT" -> "predicament"
  "RELIEF" -> "relief"
  "SHOP" -> "store"
  "SOME" -> "some"
  "SPEECH" -> "speech"
  "STREET" -> "street"
  "THIS" -> "that"
  "TREES" -> "trees"
  "WATERMELON" -> "watermelon"
  "WORDS" -> "words"
  "ROOMS" -> "rooms"
  "WORK" -> "work"
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
    else typ

renderAsWord frame = not $ isNumber $ Just frame

isSingular frame = case getType frame of
  Just "NEIGHBORS" -> False
  Just "TREES" -> False
  _ -> case fValue P.Quantifier frame >>= getType of
    Just s -> s == "1"
    _ -> case fValue P.Specifier_all frame >>= getType of
      Just "ALL" -> False
      _ -> True
