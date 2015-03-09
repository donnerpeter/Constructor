module Constructor.Coerce (asVerb, asTenseHead, asCopula, asNoun, asNegateable) where

import Constructor.Agreement
import Constructor.Constructions
import Constructor.Mite
import Constructor.LexiconUtils
import Constructor.Util
import Constructor.Variable
import Constructor.CopulaData
import qualified Constructor.SemanticProperties as P

asVerb :: Mite -> [Mite]
asVerb m = case cxt m of
  Verb {} -> [m]
  _c -> case asCopula _c of
    Just (cd, rest) -> [mite $ Verb (copula cd), mite $ CopulaHead (cd { copBound = True })] ++ copulaSem cd ++ rest
    _ -> []

asTenseHead :: Mite -> [Mite]
asTenseHead m = case cxt m of
  TenseHead {} -> [m]
  Argument Nom v -> let
    [ch@(cxt -> CopulaHead cd), tenseHead] = copulaHead NomNPCopula empty "copula" P.Arg2 Optional v
    in [tenseHead, ch]
  Argument Instr v -> let
    [ch@(cxt -> CopulaHead cd), tenseHead] = copulaHead InstrNPCopula empty "copula" P.Arg2 Obligatory v
    in [tenseHead, ch] ++ instrCopula cd
  Adj v attr Instr agr -> reverse $ copulaHead AdjCopula agr "copula" attr Obligatory v
  Adj v attr Nom agr -> reverse $ copulaHead AdjCopula agr "copula" attr Optional v
  _ -> []

instrCopula cd = [semS (copula cd) P.ProfessionCopula "true", mite $ ConjEmphasizeable (copula cd)]

asCopula :: Construction -> Maybe (CopulaData, [Mite])
asCopula = \case
  CopulaHead cd -> Just (cd, [])
  Adj v attr Nom agr -> let
    [ch@(cxt -> CopulaHead cd), tenseHead] = copulaHead AdjCopula agr "copula" attr Optional v
    in Just (cd, [tenseHead])
  Argument Nom v -> let
    [ch@(cxt -> CopulaHead cd), tenseHead] = copulaHead NomNPCopula empty "copula" P.Arg2 Optional v
    in Just (cd, [tenseHead])
  Argument Instr v -> let
    [ch@(cxt -> CopulaHead cd), tenseHead] = copulaHead InstrNPCopula empty "copula" P.Arg2 Obligatory v
    in Just (cd, [tenseHead] ++ instrCopula cd)
  _ -> Nothing

asNoun :: Construction -> Maybe (Construction, [Mite])
asNoun c = case c of
  Argument {} -> Just (c, [])
  Adj v0 attr caze agr -> let
    v = makeV v0 "noun"
    noun = v ""
    in Just (Argument caze noun, [semS noun P.Elided "true", mite $ Handicap noun, semV noun attr v0] ++ rusNumber agr noun)
  Possessive caze agr v0 -> let
    v = makeV v0 "noun"
    noun = v ""
    in Just (Argument caze noun, [semS noun P.Elided "true", mite $ Handicap noun, semV noun P.Arg1 v0] ++ rusNumber agr noun)
  _ -> Nothing

asNegateable :: Construction -> Maybe Variable
asNegateable (Negateable v) = Just v
asNegateable (NounPhrase v) = Just v
asNegateable _ = Nothing
