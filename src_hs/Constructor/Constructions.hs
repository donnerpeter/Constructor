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
data SeqData = SeqData { seqVar :: Variable, seqConj :: String, seqReady :: Bool, seqHasLeft :: Bool, seqHasRight :: Bool, seqHybrid :: Bool } deriving (Eq, Ord)
instance Show SeqData where
  show sd = show (seqVar sd) ++ " " ++ seqConj sd ++ (if seqReady sd then "" else "!ready") ++
            (if seqHasLeft sd then " left" else "") ++(if seqHasRight sd then " right" else "")++
            (if seqHybrid sd then " hybrid" else "")
data Construction = Word Variable String
                  | Sem Variable SemValue
                  | Unify Variable Variable
                  | Adj Variable ArgKind Agr
                  | CompositeAdj Variable ArgKind Agr
                  | AdjHead Variable ArgKind Agr
                  | Verb Variable
                  | NomHead Agr Variable Satisfied
                  | GenHead Variable
                  | ArgHead ArgKind Variable
                  | SemArgHead SemArgKind Variable
                  | PrepHead String ArgKind Variable
                  | SemPreposition ArgKind Variable
                  | Quantifier ArgKind Agr Variable
                  | Argument ArgKind Variable
                  | SemArgument SemArgKind {-head-} Variable {-child-} Variable
                  | Adverb Variable
                  | NounAdjunct P.VarProperty {-requires comma-} Bool Variable
                  | Elaboration Variable
                  | Unclosed Side [Variable]
                  | Closed [Variable]
                  | CompHead Variable
                  | ConditionCompHead Variable
                  | Wh Variable
                  | WhInSitu Variable
                  | ExistentialWh {-wh-} Variable {-tensed-} Variable
                  | WhAsserter Variable
                  | UniversalPronoun Variable
                  | NegativePronoun Variable
                  | QuestionVariants Variable ArgKind
                  | Conjunction SeqData
                  | SeqLeft Construction
                  | SeqRight Construction
                  | Clause Variable
                  | TopLevelQuestion Variable
                  | Possessive ArgKind Agr Variable
                  | EmptyCxt Construction
                  | Diversifier Int
                  | Tense Variable
                  | TenseHead Variable
                  | FutureTense Agr Variable
                  | Copula Variable
                  | ShortAdj Variable
                  | ConditionComp Variable {-if/when-} String {-has cp-} Bool
                  | CommaSurrounded {-opened-} Bool {-closed-} Bool Construction
                  | DashSurrounded {-opened-} Bool {-closed-} Bool Construction
                  | Control Variable
                  | ModalityInfinitive {-modality-} Variable {-cp-} Variable
                  | ControlledInfinitive Variable
                  | RelativeHead Variable
                  | RelativeClause Variable
                  | Complement Variable
                  | Complementizer Variable
                  | SurroundingComma Variable
                  | SurroundingDash Variable
                  | Colon {-role-} String Variable
                  | Quote Variable {-closing-} Bool
                  | QuotedWord Construction {-closed-} Bool
                  | VerbalModifier P.VarProperty {-requires comma-} Bool Variable
                  | DirectSpeechHead Variable {--child--} (Maybe Variable)
                  | DirectSpeech Variable
                  | DirectSpeechDash Variable
                  | Ellipsis Variable {-left-} (Maybe Construction) {-right-} (Maybe Construction)
                  | RaisingVerb {-verb-} Variable {-subj-} Variable
                  | Raiseable Agr Variable
                  | TwoWordCxt String {-first-} Bool [Construction] Variable
                  | ReasonComp Variable {-has cp-} Bool
                  | ReflexiveReference Variable
                  | ReflexiveTarget Variable
                  | Sentence Variable
                  | ConjEmphasis P.StrProperty Variable
                  | Negated Variable
                  -- | S1 | S2 | S3 | S4
                  deriving (Show, Ord, Eq)

isHappy cxt = case cxt of
  Adj {} -> False; Adverb {} -> False; NounAdjunct {} -> False
  ArgHead {} -> False; PrepHead {} -> False; SemPreposition {} -> False; Argument {} -> False;
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
  Clause {} -> False; TopLevelQuestion {} -> False
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
  _ -> True

getCommaSurroundableVar cxt = case cxt of
  ConditionComp v _ True -> Just v
  ReasonComp v True -> Just v
  Complement v -> Just v
  RelativeClause v -> Just v
  VerbalModifier _ True v -> Just v
  NounAdjunct _ True v -> Just v
  Argument _ v -> Just v
  _ -> Nothing

isStable (Conjunction sd) = seqHasLeft sd == seqHasRight sd || seqConj sd == ","
isStable (Ellipsis _ leftAnchor rightAnchor) = isJust leftAnchor == isJust rightAnchor
isStable _ = True
