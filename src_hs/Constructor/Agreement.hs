module Constructor.Agreement where

import Control.Exception
import Data.Maybe

data Gender = Masc | Fem | Neu deriving (Show, Eq, Ord)
data RusNumber = Sg | Pl deriving (Show, Eq, Ord)
type Person = Int

data Agr = Agr { gender::Maybe Gender, number::RusNumber, person::Maybe Person } deriving (Eq, Ord)
instance Show Agr where
  show (Agr {gender=g, number=n, person=p}) = (if isNothing g then "" else show $ fromJust g) ++ show n ++ (if isNothing p then "" else show $ fromJust p)

agree a1 a2 = adjAgree a1 a2 && number a1 == number a2

adjAgree (Agr {gender=g1, person=p1}) (Agr {gender=g2, person=p2}) =
  (p1 == Nothing || p2 == Nothing || p1 == p2) &&
  (g1 == Nothing || g2 == Nothing || g1 == g2)

commonAgr a1@(Agr {gender=g1, number=n1, person=p1}) a2@(Agr {gender=g2, number=n2, person=p2}) =
  assert (agree a1 a2) $ Agr (if g1 == Nothing then g2 else g1) n1 (if p1 == Nothing then p2 else p1)

commonAdjAgr a1@(Agr {gender=g1, number=n1, person=p1}) a2@(Agr {gender=g2, number=n2, person=p2}) =
  assert (adjAgree a1 a2) $ Agr (if g1 == Nothing then g2 else g1) Pl (if p1 == Nothing then p2 else p1)

m = Agr (Just Masc) Sg Nothing
f = Agr (Just Fem) Sg Nothing
n = Agr (Just Neu) Sg Nothing
sg = Agr Nothing Sg Nothing
pl = Agr Nothing Pl Nothing
m3 = Agr (Just Masc) Sg (Just 3)
f3 = Agr (Just Fem) Sg (Just 3)
n3 = Agr (Just Neu) Sg (Just 3)
pl1 = Agr Nothing Pl (Just 1)
pl2 = Agr Nothing Pl (Just 2)
pl3 = Agr Nothing Pl (Just 3)
sg3 = Agr Nothing Sg (Just 3)