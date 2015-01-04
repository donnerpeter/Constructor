module Constructor.Constructions where

import Constructor.Agreement
import Constructor.Variable
import Constructor.Util
import Data.Maybe
import qualified Constructor.SemanticProperties as P

cases = [Nom,Acc,Gen,Dat,Instr,Prep]

data ArgKind = Nom | Acc | Gen | Dat | Instr | Prep | PP String ArgKind | ScalarAdverb deriving (Show, Eq, Ord)
data SemArgKind = Direction deriving (Show, Eq, Ord)

data Satisfied = Unsatisfied | Satisfied deriving (Show, Eq, Ord)
data SubjectKind = FiniteSubject | CopulaSubject | NPCopulaSubject deriving (Show, Eq, Ord)
data CopulaKind = NPCopula | PPCopula deriving (Show, Eq, Ord)

data SeqData = SeqData { seqVar :: Variable, seqConj :: String, seqReady :: Bool, seqHasLeft :: Bool, seqHasRight :: Bool, seqHybrid :: Bool } deriving (Eq, Ord)
instance Show SeqData where
  show sd = show (seqVar sd) ++ " " ++ seqConj sd ++ (if seqReady sd then "" else "!ready") ++
            (if seqHasLeft sd then " left" else "") ++(if seqHasRight sd then " right" else "")++
            (if seqHybrid sd then " hybrid" else "")
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
                  | Closed [Variable]

                  -- auxiliary
                  | EmptyCxt Construction
                  | Diversifier Int

                  --Russian:
                  --adjectives
                  | Adj Variable ArgKind Agr
                  | CompositeAdj Variable ArgKind Agr
                  | AdjHead Variable ArgKind Agr
                  | ShortAdj Variable

                  -- arguments
                  | NomHead Agr Variable Satisfied SubjectKind
                  | GenHead Variable
                  | ArgHead ArgKind Variable
                  | SemArgHead SemArgKind Variable
                  | PrepHead String ArgKind Variable
                  | Argument ArgKind Variable
                  | SemArgument SemArgKind {-head-} Variable {-child-} Variable
                  | Possessive ArgKind Agr Variable

                  -- adjuncts
                  | NounPhrase Variable
                  | Adverb Variable
                  | NounAdjunct P.VarProperty {-requires comma-} Bool Variable
                  | VerbalModifier P.VarProperty {-requires comma-} Bool Variable

                  -- subordinate clauses
                  | CompHead Variable
                  | ConditionCompHead Variable
                  | RelativeClause Agr Variable
                  | Relativizer Variable
                  | Complement Variable
                  | Complementizer Variable
                  | ConditionComp Variable {-if/when-} String {-has cp-} Bool
                  | ReasonComp Variable {-has cp-} Bool

                  -- verbs & clauses
                  | Verb Variable -- finite or infinitive or participle
                  | Clause Variable -- a finite clause, possibly a well-formed copula
                  | Tense Variable
                  | TenseHead Variable
                  | FutureTense Agr Variable
                  | CopulaHead CopulaKind Agr {-subj-} Variable {-copula-} Variable {-cp-} Variable

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

                  -- other
                  | Control Variable
                  | ModalityInfinitive {-modality-} Variable {-cp-} Variable
                  | ControlledInfinitive Variable
                  | DirectSpeechHead Variable {--child--} (Maybe Variable)
                  | DirectSpeech Variable
                  | DirectSpeechDash Variable
                  | Ellipsis Variable {-left-} (Maybe Construction) {-right-} (Maybe Construction)
                  | RaisingVerb {-verb-} Variable {-subj-} Variable
                  | Raiseable Agr Variable
                  | TwoWordCxt String {-first-} Bool [Construction] Variable
                  | ReflexiveReference Variable
                  | ReflexiveTarget Variable
                  | ConjEmphasis P.StrProperty Variable
                  | Negated Variable
                  | SemPreposition ArgKind Variable
                  | Quantifier ArgKind Agr Variable
                  | Elaboration Variable
                  deriving (Show, Ord, Eq)

isHappy cxt = case cxt of
  Adj {} -> False; Adverb {} -> False; NounAdjunct {} -> False
  ArgHead {} -> False; PrepHead {} -> False; SemPreposition {} -> False; Argument {} -> False;
  NomHead _ _ Unsatisfied kind -> kind == FiniteSubject
  SemArgHead {} -> False; SemArgument {} -> False
  Quantifier {} -> False
  CompHead {} -> False; ConditionCompHead {} -> False; ConditionComp {} -> False; ReasonComp {} -> False
  Elaboration {} -> False
  Conjunction sd -> seqHasLeft sd && seqHasRight sd
  SeqRight (Wh {}) -> False
  GenHead {} -> False; Possessive {} -> False
  Tense {} -> False
  FutureTense {} -> False
  CommaSurrounded {} -> False; SurroundingComma {} -> False
  DashSurrounded {} -> False; SurroundingDash {} -> False
  ControlledInfinitive {} -> False; Control {} -> False
  ModalityInfinitive {} -> False
  ExistentialWh {} -> False; WhAsserter {} -> False
  CopulaHead {} -> False;
  TopLevelQuestion {} -> False
  QuotedWord _ False -> False; Quote _ False -> False
  DirectSpeechDash {} -> False; DirectSpeechHead _ Nothing -> False
  Colon {} -> False
  VerbalModifier {} -> False
  RaisingVerb {} -> False; Raiseable {} -> False
  TwoWordCxt {} -> False
  Ellipsis {} -> False
  Unclosed {} -> False
  Complement {} -> False
  Wh {} -> False; WhInSitu {} -> False
  RelativeClause {} -> False
  Word {} -> False
  _ -> True

getCommaSurroundableVar cxt = case cxt of
  ConditionComp v _ True -> Just v
  ReasonComp v True -> Just v
  Complement v -> Just v
  RelativeClause _ v -> Just v
  VerbalModifier _ True v -> Just v
  NounAdjunct _ True v -> Just v
  Argument _ v -> Just v
  _ -> Nothing

isStable (Conjunction sd) = seqHasLeft sd == seqHasRight sd || seqConj sd == ","
isStable (Ellipsis _ leftAnchor rightAnchor) = isJust leftAnchor == isJust rightAnchor
isStable _ = True
