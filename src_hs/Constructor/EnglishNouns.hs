module Constructor.EnglishNouns (noun, isSingular, renderAsWord) where

import Constructor.Sense
import Data.Maybe
import Constructor.Util

noun Nothing _ = "??"
noun (Just typ) frame = case typ of
  "CASE" -> "thing"
  "ME" -> "me"
  "HE" -> "he"
  "NEIGHBORS" -> "neighbors"
  "NEIGHBOR" -> "neighbor"
  "TREES" -> "trees"
  "WATERMELON" -> "watermelon"
  "MATTER" -> "matter"
  "AMAZE" -> "amazement"
  "ORDER" -> "order"
  "COUNTING" -> if isJust (usage "domain" frame) then "count" else "counting"
  "CASHIER" -> "cashier"
  "WORDS" -> "words"
  "PREDICAMENT" -> "predicament"
  "JOY" -> "joy"
  "RELIEF" -> "relief"
  "SPEECH" -> "speech"
  "SHOP" -> "store"
  "CORNER" -> "corner"
  "STREET" -> "street"
  "MONEY" -> "money"
  "HAMMER" -> "hammer"
  "MOUTH" -> "mouth"
  "NOSE" -> "nose"
  "OPINION" -> "opinion"
  "MEANING" -> "meaning"
  "SOME" -> "some"
  "OTHERS" -> "others"
  "CHILD" -> "child"
  "BENCH" -> "bench"
  "FINGER" -> if isSingular frame then "finger" else "fingers"
  "JAW" -> if isSingular frame then "jaw" else "jaws"
  "JAWS" -> "jaws"
  "ROOMS" -> "rooms"
  "APARTMENTS" -> "apartments"
  "OFFICES" -> "offices"
  "WORK" -> "work"
  "ARGUE" -> "argument"
  "THIS" -> "that"
  "NAMED_PERSON" -> fromMaybe "??name" $ sValue "name" frame
  "GARDEN" -> if (fValue "name" frame >>= getType) == Just "летний" then "Summer Garden" else "garden"
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
      _ -> typ
    else typ

renderAsWord frame = not $ isNumber $ Just frame

isSingular frame = case getType frame of
  Just "NEIGHBORS" -> False
  Just "TREES" -> False
  _ -> case fValue "quantifier" frame >>= getType of
    Just s -> s == "1"
    _ -> True
