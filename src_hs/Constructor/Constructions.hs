module Constructor.Constructions where

import Constructor.Agreement
import Constructor.Variable
import Data.Maybe

data ArgKind = Nom | Acc | Gen | Dat | Instr | Prep | PP String ArgKind | 
               ClauseArg ClauseForce | CP | AdjKind ArgKind Agr | ScalarAdverb
               deriving (Show, Eq, Ord)
data ClauseForce = Declarative | Interrogative deriving (Show, Eq, Ord)
data SeqData = SeqData { seqVar :: Variable, seqConj :: String, seqReady :: Bool,
                         seqKind :: Maybe ArgKind, seqHasLeft :: Bool, seqHasRight :: Bool } deriving (Eq, Ord)
instance Show SeqData where
  show sd = show (seqVar sd) ++ " " ++ seqConj sd ++
            (if isJust $ seqKind sd then " (" ++ show (fromJust $ seqKind sd) ++ ")" else "") ++
            (if seqReady sd then "" else "!ready") ++ (if seqHasLeft sd then " left" else "") ++
            (if seqHasRight sd then " right" else "")
data Construction = Word Variable String
                  | Sem Variable String SemValue
                  | Unify Variable Variable
                  | Adj Variable ArgKind Agr
                  | CompositeAdj Variable ArgKind Agr
                  | AdjHead Variable ArgKind Agr
                  | Verb Variable
                  | NomHead Agr Variable {-satisfied-} Bool
                  | GenHead Variable
                  | ArgHead ArgKind Variable
                  | PrepHead String ArgKind Variable
                  | PrepositionActivator String ArgKind [Construction]
                  | ActivePreposition Variable
                  | Argument ArgKind Variable 
                  | Adverb String String
                  | NounAdjunct Variable
                  | Elaboration Variable
                  | Unclosed Construction
                  | CompHead Variable
                  | ConditionCompHead Variable
                  | Wh Variable Variable
                  | QuestionVariants (Maybe Variable) (Maybe String)
                  | Conjunction SeqData
                  | Clause ClauseForce Variable
                  | TopLevelQuestion Variable
                  | ElidedArgHead Construction
                  | Possessive ArgKind Agr Variable
                  | EmptyCxt Construction
                  | CopulaTense Variable
                  | Copula Variable
                  | ShortAdj Variable
                  | ConditionComp Variable {-if/when-} String {-has cp-} Bool
                  | CommaSurrounded {-opened-} Bool {-closed-} Bool Construction
                  | Control Variable
                  | ModalityInfinitive Variable
                  | ControlledInfinitive Variable
                  | RelativeHead Variable
                  | RelativeClause Variable
                  | Complement Variable
                  | Complementizer Variable
                  | SurroundingComma {-closing-} Bool Variable
                  | Colon {-role-} String Variable
                  | Quote Variable {-closing-} Bool
                  | QuotedWord Construction {-closed-} Bool
                  | VerbalModifier {-attr-} String {-requires comma-} Bool Variable
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
                  | ConjEmphasis String Variable
                  -- | S1 | S2 | S3 | S4
                  deriving (Show, Ord, Eq)

isHappy cxt = case cxt of
  Adj {} -> False; Adverb {} -> False; NounAdjunct {} -> False
  ArgHead {} -> False; PrepHead {} -> False; Argument {} -> False; ElidedArgHead {} -> False
  CompHead {} -> False; ConditionCompHead {} -> False; ConditionComp {} -> False; ReasonComp {} -> False
  Elaboration {} -> False
  Conjunction (SeqData {seqHasLeft = seqHasLeft, seqHasRight = seqHasRight}) -> seqHasLeft && seqHasRight
  NomHead _ _ satisfied -> satisfied
  GenHead {} -> False; Possessive {} -> False
  CopulaTense {} -> False
  CommaSurrounded {} -> False; SurroundingComma {} -> False
  ControlledInfinitive {} -> False; Control {} -> False
  Clause {} -> False; TopLevelQuestion {} -> False
  QuotedWord _ False -> False; Quote _ False -> False
  DirectSpeechDash {} -> False; DirectSpeechHead _ Nothing -> False
  Colon {} -> False
  VerbalModifier {} -> False
  PrepositionActivator {} -> False; ActivePreposition {} -> False
  RaisingVerb {} -> False; Raiseable {} -> False
  TwoWordCxt {} -> False
  Ellipsis {} -> False
  Unclosed {} -> False
  Complement {} -> False
  Wh {} -> False
  RelativeClause {} -> False
  _ -> True

isCommaSurroundable cxt = case cxt of
  ConditionComp _ _ True -> True; ReasonComp _ True -> True
  Complement {} -> True
  RelativeClause {} -> True
  VerbalModifier _ True _ -> True
  _ -> False