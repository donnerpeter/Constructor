module Constructor.Agreement where

import Control.Exception
import Data.Maybe

data Gender = Masc | Fem | Neu deriving (Show, Eq, Ord)
data RusNumber = Sg | Pl deriving (Show, Eq, Ord)
type Person = Int

data Agr = Agr { gender::Maybe Gender, number::Maybe RusNumber, person::Maybe Person } deriving (Eq, Ord)
instance Show Agr where
  show agr = (fromMaybe "" $ fmap show $ gender agr) ++ (fromMaybe "" $ fmap show $ number agr) ++ (fromMaybe "" $ fmap show $ person agr)

agree a1 a2 = adjAgree a1 a2 && (number a1 == Nothing || number a2 == Nothing || number a1 == number a2)

adjAgree (Agr {gender=g1, person=p1}) (Agr {gender=g2, person=p2}) =
  (p1 == Nothing || p2 == Nothing || p1 == p2) &&
  (g1 == Nothing || g2 == Nothing || g1 == g2)

unifyAttrs f a1 a2 = if isJust m1 then m1 else m2 where m1 = f a1; m2 = f a2

commonAgr a1 a2 = assert (agree a1 a2) $ Agr (unifyAttrs gender a1 a2) (unifyAttrs number a1 a2) (unifyAttrs person a1 a2)

m = Agr (Just Masc) (Just Sg) Nothing
f = Agr (Just Fem) (Just Sg) Nothing
n = Agr (Just Neu) (Just Sg) Nothing
sg = Agr Nothing (Just Sg) Nothing
pl = Agr Nothing (Just Pl) Nothing
m3 = Agr (Just Masc) (Just Sg) (Just 3)
f3 = Agr (Just Fem) (Just Sg) (Just 3)
n3 = Agr (Just Neu) (Just Sg) (Just 3)
pl1 = Agr Nothing (Just Pl) (Just 1)
pl2 = Agr Nothing (Just Pl) (Just 2)
pl3 = Agr Nothing (Just Pl) (Just 3)
sg3 = Agr Nothing (Just Sg) (Just 3)
empty = Agr Nothing Nothing Nothing