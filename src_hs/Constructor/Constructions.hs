module Constructor.Constructions where

import Constructor.Agreement
import Constructor.Variable
import Constructor.Util
import Constructor.CopulaData
import Data.Maybe
import qualified Constructor.SemanticProperties as P

cases = [Nom,Acc,Gen,Dat,Instr,Prep]

data ArgKind = Nom | Acc | Gen | Dat | Instr | Prep | PP String ArgKind | ScalarAdverb deriving (Show, Eq, Ord)
data SemArgKind = Direction deriving (Show, Eq, Ord)
data Optionality = Optional | Obligatory deriving (Show, Eq, Ord)

data Satisfied = Unsatisfied | Satisfied deriving (Show, Eq, Ord)

data SeqData = SeqData { seqVar :: Variable, seqConj :: String, seqReady :: Bool, seqHasLeft :: Bool, seqHasRight :: Bool, seqHybrid :: Bool, seqHasComma:: Bool } deriving (Eq, Ord)

instance Show SeqData where
  show sd = show (seqVar sd) ++ " " ++ seqConj sd ++ (if seqReady sd then "" else "!ready") ++
            (if seqHasLeft sd then " left" else "") ++(if seqHasRight sd then " right" else "")++
            (if seqHybrid sd then " hybrid" else "")++
            (if seqHasComma sd then " hasComma" else "")

data Construction = Word Variable String
                  --semantic
                  | Sem Variable SemValue
                  | Unify Variable Variable

                  -- punctuation
                  | SurroundingComma Variable
                  | SurroundingDash Variable
                  | Colon {-role-} String Variable
                  | Quote Variable {-closing-} Bool
                  | QuotedWord Construction {-closed-} Bool
                  | CommaSurrounded {-opened-} Bool {-closed-} Bool Construction
                  | DashSurrounded {-opened-} Bool {-closed-} Bool Construction
                  | Sentence Variable
                  | Unclosed Side [Variable]
                  | Handicap Variable

                  -- auxiliary
                  | EmptyCxt Construction
                  | Diversifier Int

                  --Russian:
                  --adjectives
                  | Adj Variable P.VarProperty ArgKind Agr
                  | AdjHead Variable ArgKind Agr
                  | ShortAdj Agr P.VarProperty Variable
                  | ComparativeAdj P.VarProperty Variable
                  | ComparativeEmphasis Variable

                  -- arguments
                  | NomHead Agr Variable Satisfied
                  | GenHead [(P.VarProperty, Variable)]
                  | ArgHead ArgKind P.VarProperty Variable
                  | SemArgHead Optionality SemArgKind Variable
                  | PrepHead String ArgKind Variable
                  | Argument ArgKind Variable
                  | SemArgument SemArgKind {-head-} Variable {-child-} Variable
                  | Possessive ArgKind Agr Variable

                  -- adjuncts
                  | NounPhrase Variable
                  | Adverb Variable
                  | AdverbModifiable Variable
                  | ModifierAdverb Variable
                  | NounAdjunct P.VarProperty {-requires comma-} Bool Variable
                  | VerbalModifier P.VarProperty {-requires comma-} Bool Variable

                  -- subordinate clauses
                  | CompHead P.VarProperty Variable
                  | ConditionCompHead Variable
                  | RelativeClause Agr Variable
                  | Relativizer Variable
                  | Complement Variable
                  | Complementizer Variable
                  | ComparisonAnchor Satisfied Variable
                  | Comparativizer Variable
                  | ComparativeHead Variable
                  | ConditionComp Variable {-if/when-} String {-has cp-} Bool
                  | ReasonComp Variable {-has cp-} Bool

                  -- verbs & clauses
                  | Verb Variable -- finite or infinitive or participle
                  | Clause Variable -- a finite clause, possibly a well-formed copula
                  | Tense Variable
                  | TenseHead Optionality Variable
                  | FutureTense Agr Variable
                  | CopulaHead CopulaData

                  -- wh-related
                  | Wh Agr Variable -- a phrase with wh inside
                  | WhLeaf Variable -- wh-word
                  | WhInSitu Variable
                  | ExistentialWh {-wh-} Variable {-tensed-} Variable
                  | WhAsserter Variable
                  | UniversalPronoun Variable
                  | NegativePronoun Variable
                  | QuestionVariants Variable ArgKind
                  | TopLevelQuestion Variable

                  -- conjunction
                  | Conjunction SeqData
                  | SeqLeft Construction
                  | SeqRight Construction

                  -- negation
                  | Negated Variable
                  | PendingNegation Variable
                  | NegationModifier Variable

                  -- other
                  | Control Variable
                  | ModalityInfinitive {-modality-} Variable {-cp-} Variable
                  | ControlledInfinitive Variable
                  | DirectSpeechHead Variable {--child--} (Maybe Variable)
                  | DirectSpeechDash Variable
                  | Ellipsis Variable {-right-} (Maybe Construction)
                  | RaisingVerb {-verb-} Variable {-subj-} Variable
                  | TwoWordCxt String {-first-} Bool [Construction] Variable
                  | ReflexiveReference Variable
                  | ReflexiveTarget Variable
                  | ConjEmphasis P.StrProperty Variable
                  | AndEmphasis Variable
                  | ConjEmphasizeable Variable
                  | ParticleEmphasizeable Variable
                  | SemPreposition ArgKind Variable
                  | Quantifier ArgKind ArgKind Agr Variable
                  | Elaboration Variable
                  deriving (Show, Ord, Eq)

isHappy cxt = case cxt of
  Sem {} -> True; Unify {} -> True; EmptyCxt {} -> True; Diversifier {} -> True
  Verb {} -> True; Clause {} -> True; AdverbModifiable {} -> True
  ConjEmphasizeable {} -> True; ParticleEmphasizeable {} -> True
  Sentence {} -> True
  NomHead {} -> True; GenHead {} -> True
  ReflexiveReference {} -> True; ReflexiveTarget {} -> True
  AdjHead {} -> True; NounPhrase {} -> True
  SemArgHead Optional _ _ -> True
  TenseHead Optional _ -> True
  QuestionVariants {} -> True
  WhLeaf {} -> True
  NegativePronoun {} -> True; UniversalPronoun {} -> True
  Negated {} -> True
  Conjunction sd -> seqHasLeft sd && seqHasRight sd
  SeqRight (Wh {}) -> False; SeqRight _ -> True
  SeqLeft _ -> True
  Handicap _ -> True
  Unclosed {} -> True
  ComparativeHead {} -> True
  _ -> False

getCommaSurroundableVar cxt = case cxt of
  ConditionComp v _ True -> Just v
  ReasonComp v True -> Just v
  Complement v -> Just v
  RelativeClause _ v -> Just v
  VerbalModifier _ True v -> Just v
  NounAdjunct _ True v -> Just v
  Argument _ v -> Just v
  ComparisonAnchor _ v -> Just v
  _ -> Nothing

isStable (Conjunction sd) = seqHasLeft sd == seqHasRight sd || seqConj sd == ","
isStable (Ellipsis _ rightAnchor) = isNothing rightAnchor
isStable _ = True
