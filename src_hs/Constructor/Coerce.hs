module Constructor.Coerce (asVerb, asTenseHead, asCopula) where

import Constructor.Agreement
import Constructor.Constructions
import Constructor.Mite
import Constructor.LexiconUtils
import Constructor.CopulaData
import qualified Constructor.SemanticProperties as P

asVerb :: Mite -> [Mite]
asVerb m = case cxt m of
  Verb {} -> [m]
  CopulaHead cd -> [mite $ Verb (copula cd), mite $ CopulaHead (cd { copBound = True })] ++ copulaSem cd
  _ -> []

asTenseHead :: Mite -> [Mite]
asTenseHead m = case cxt m of
  TenseHead {} -> [m]
  Argument Instr v -> let
    [ch@(cxt -> CopulaHead cd), tenseHead] = copulaHead NPCopula empty "copula" P.Arg2 Obligatory v
    in [tenseHead, ch] ++ instrCopula cd
  Adj v attr Instr agr -> reverse $ copulaHead AdjCopula agr "copula" attr Obligatory v
  _ -> []

instrCopula cd = [semS (copula cd) P.ProfessionCopula "true", mite $ ConjEmphasizeable (copula cd)]

asCopula :: Construction -> Maybe (CopulaData, [Mite])
asCopula = \case
  CopulaHead cd -> Just (cd, [])
  Argument Instr v -> let
    [ch@(cxt -> CopulaHead cd), tenseHead] = copulaHead NPCopula empty "copula" P.Arg2 Obligatory v
    in Just (cd, [tenseHead] ++ instrCopula cd)
  _ -> Nothing
