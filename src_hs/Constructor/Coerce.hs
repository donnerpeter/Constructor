module Constructor.Coerce (asVerb, asTenseHead, asCopula, asNoun, asNegateable, asClause) where

import Constructor.Agreement
import Constructor.Mite
import Constructor.LexiconUtils
import Constructor.Util
import Constructor.Variable
import Constructor.CopulaData
import Constructor.Ellipsis
import Constructor.InteractionEnv
import Data.List (partition)
import qualified Constructor.SemanticProperties as P

asVerb :: InteractionEnv -> Mite -> [Mite]
asVerb env m = case cxt m of
  Verb {} -> [m]
  _c -> case asCopula _c of
      Just (cd, rest) -> let
        verb = copula cd
        copulaCoercion = [mite $ Verb verb, mite $ CopulaHead (cd { copBound = True })] ++ copulaSem cd ++ rest
        raiseToEllipsis anchor var = let
          ellipsisVar = makeV var "ell" ""
          ellipses = suggestSingleAnchorEllipsis env ellipsisVar anchor
          in if null ellipses then []
             else [mite $ Verb verb, mite $ Clause ellipsisVar, mite $ Handicap ellipsisVar,
                   semV ellipsisVar P.EllipsisAnchor2 var, semS ellipsisVar P.Elided "true"]
                  ++ xor (map (\(ClauseEllipsis v mites) -> [mite $ Unify v verb, mite $ Verb v] ++ mites) ellipses)
        ellipsis = case _c of
          Argument _ v -> raiseToEllipsis _c v
          Adj v _ _ _ -> let
            plain = raiseToEllipsis _c v
            Just (arg, rest) = asNoun _c
            withNounCoercion = raiseToEllipsis arg v
            in if null plain then withNounCoercion ++ rest else plain
          _ -> []
        in xorNonEmpty $ [copulaCoercion, ellipsis]
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
  ShortAdj agr attr v -> reverse $ copulaHead AdjCopula agr "copula" attr Optional v
  ComparativeAdj attr v -> reverse $ copulaHead AdjCopula empty "copula" attr Optional v
  _ -> []

instrCopula cd = [semS (copula cd) P.ProfessionCopula "true", mite $ ConjEmphasizeable (copula cd)]

asCopula :: Construction -> Maybe (CopulaData, [Mite])
asCopula = \case
  CopulaHead cd -> Just (cd, [])
  Adj v attr Nom agr -> let
    [ch@(cxt -> CopulaHead cd), tenseHead] = copulaHead AdjCopula agr "copula" attr Optional v
    in Just (cd, [tenseHead])
  ShortAdj agr attr v -> let
    [ch@(cxt -> CopulaHead cd), tenseHead] = copulaHead AdjCopula agr "copula" attr Optional v
    in Just (cd, [tenseHead])
  ComparativeAdj attr v -> let
    [ch@(cxt -> CopulaHead cd), tenseHead] = copulaHead AdjCopula empty "copula" attr Optional v
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
    in Just (Argument caze noun, [semS noun P.ElidedNoun "true", mite $ Handicap noun, semV noun attr v0] ++ rusNumber agr noun)
  Possessive caze agr v0 -> let
    v = makeV v0 "noun"
    noun = v ""
    in Just (Argument caze noun, [semS noun P.ElidedNoun "true", mite $ Handicap noun, semV noun P.Arg1 v0] ++ rusNumber agr noun)
  _ -> Nothing

asNegateable :: Construction -> Maybe (Variable, [Mite])
asNegateable (NounPhrase v) = Just (v, [])
asNegateable c@(Tense v) = Just (v, [mite c])
asNegateable c@(Adj v _ _ _) = Just (v, [mite c])
asNegateable c@(Possessive _ _ v) = Just (v, [mite c])
asNegateable c@(ShortAdj _ _ v) = Just (v, [mite c])
asNegateable c@(ComparativeAdj _ v) = Just (v, [mite c])
asNegateable _ = Nothing

asClause :: Construction -> Maybe (Variable, [Mite])
asClause = \case
  Clause cp -> Just (cp, [])
  TopLevelQuestion cp -> Just (cp, [semS cp P.ExclamativeQuestion "true"])
  _ -> Nothing
