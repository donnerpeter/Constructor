module Constructor.EnglishPronouns where

import Constructor.Sense
import Constructor.Inference
import Constructor.Util
import Data.Maybe
import qualified Constructor.SemanticProperties as P

isPronoun frame = hasAnyType ["ME", "YOU", "THEY", "HE", "SHE", "WE", "wh"] frame

npPronoun nom frame = case getType frame of
  Just "ME" -> if nom then "I" else "me"
  Just "WE" -> if nom then "we" else "us"
  Just "YOU" -> "you"
  Just "THEY" -> if nom then "they" else "them"
  Just s ->
    if isHuman $ resolve frame then
      if s == "HE" then if nom then "he" else "him"
      else if s == "SHE" then if nom then "she" else "her"
      else s
    else "it"
  _ -> "???"

whWord nom frame =
  if isJust (usage P.Goal frame) then "where"
  else if isJust (usage P.VTime frame) then "when"
  else if isJust (usage P.Location frame) then "where"
  else if isAnimate frame then
    if Just "true" == sValue P.Negated frame then "nobody"
    else if Just "true" == (usage P.Arg2 frame >>= sValue P.ProfessionCopula) then "what"
    else if nom then "who" else "whom"
  else if isJust $ usage P.Questioned frame >>= usage P.Relative then "that"
  else "what"
