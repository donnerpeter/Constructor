module Constructor.Constructions where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Constructor.LinkedSet as LS
import Control.Exception (assert)
import Constructor.Agreement
import Constructor.Variable

data ArgKind = Nom | Acc | Gen | Dat | Instr | Prep | PP String ArgKind | CP ClauseLevel | PossKind ArgKind Agr | ScalarAdverb deriving (Show, Eq, Ord)
data ClauseLevel = TopLevel | Subordinate deriving (Show, Eq, Ord)
data Construction = Word Variable String
                  | Sem Variable String SemValue
                  | Unify Variable Variable
                  | Adj Variable ArgKind Agr
                  | AdjHead Variable ArgKind Agr
                  | Verb Variable
                  | NomHead Agr Variable
                  | GenHead Variable
                  | ArgHead ArgKind Variable
                  | PrepHead String ArgKind Variable
                  | PrepositionActivator String ArgKind [Construction]
                  | ActivePreposition Variable
                  | Argument ArgKind Variable 
                  | Adverb String String
                  | Elaboration Variable
                  | Unclosed Construction
                  | CompHead Variable
                  | ConditionCompHead Variable
                  | Wh Variable Variable
                  | QuestionVariants (Maybe Variable) (Maybe String)
                  | Conjunction Variable String {-ready-} Bool
                  | SeqRight Variable ArgKind String
                  | SeqFull Variable String
                  | Clause ClauseLevel Variable
                  | Fact Variable
                  | ElidedArgHead Construction
                  | Question Variable Variable
                  | Possessive ArgKind Agr Variable
                  | EmptyCxt Construction
                  | CopulaTense Variable
                  | Copula Variable
                  | PrepCopula Variable
                  | ShortAdj Variable
                  | ConditionComp Variable {-if/when-} String {-has cp-} Bool
                  | CommaSurrounded {-opened-} Bool {-closed-} Bool Construction
                  | Control Variable
                  | ModalityInfinitive Variable
                  | ControlledInfinitive Variable
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
                  | TwoWordCxt String {-first-} Bool [Mite] Variable
                  | ReasonComp Variable {-has cp-} Bool
                  | ReflexiveReference Variable
                  | ReflexiveTarget Variable
                  -- | S1 | S2 | S3 | S4
                  deriving (Show, Ord, Eq)
data Mite = Mite { cxt :: Construction, happy :: Bool, contradictors :: Set.Set Construction, baseMites :: [Mite] } deriving (Ord, Eq)
instance Show Mite where
  show (Mite {cxt=c, happy=h, contradictors=cc, baseMites = b}) =
    (if h then "" else "!") ++ show c -- ++ (if Set.null cc then "" else "(xor "++(show cc)++")")
      -- ++ (if null b then "" else "(base " ++ show b ++ ")")
  
isHappy (Adj {}) = False
isHappy (Adverb {}) = False
isHappy (ArgHead {}) = False
isHappy (PrepHead {}) = False
isHappy (CompHead {}) = False
isHappy (ConditionCompHead {}) = False
isHappy (Argument {}) = False
isHappy (Elaboration {}) = False
isHappy (SeqRight {}) = False
isHappy (Conjunction {}) = False
isHappy (NomHead {}) = False
isHappy (GenHead {}) = False
isHappy (ElidedArgHead {}) = False
isHappy (Possessive {}) = False
isHappy (CopulaTense {}) = False
isHappy (Copula {}) = False
isHappy (ConditionComp {}) = False
isHappy (CommaSurrounded {}) = False
isHappy (ControlledInfinitive {}) = False
isHappy (Control {}) = False
isHappy (SurroundingComma {}) = False
isHappy (Clause Subordinate _) = False
isHappy (QuotedWord _ False) = False
isHappy (Quote _ False) = False
isHappy (DirectSpeechHead _ Nothing) = False
isHappy (Colon {}) = False
isHappy (VerbalModifier {}) = False
isHappy (PrepositionActivator {}) = False
isHappy (ActivePreposition {}) = False
isHappy (DirectSpeechDash {}) = False
isHappy (RaisingVerb {}) = False
isHappy (Raiseable {}) = False
isHappy (TwoWordCxt {}) = False
isHappy (ReasonComp {}) = False
isHappy (Ellipsis {}) = False
isHappy (Unclosed {}) = False
isHappy _ = True

isCommaSurroundable (ConditionComp _ _ True) = True
isCommaSurroundable (ReasonComp _ True) = True
isCommaSurroundable (Wh {}) = True
isCommaSurroundable (Complementizer {}) = True
isCommaSurroundable (VerbalModifier _ True _) = True
isCommaSurroundable _ = False

mite cxt = Mite cxt (isHappy cxt) Set.empty []
  
semS var prop value = mite $ Sem var prop (StrValue value)
semV var prop value = mite $ Sem var prop (VarValue value)
semT var _type = semS var "type" _type

xor :: [[Mite]] -> [Mite]
xor miteGroups =
  let cxtGroups = map (map cxt) miteGroups
      allCxts = LS.removeDups $ concat cxtGroups
      allCxtSet = Set.fromList $ concat cxtGroups
      cxt2ExistingContras = Map.fromListWith Set.union [(cxt mite, contradictors mite) | mite <- concat miteGroups]
      cxt2Groups = Map.fromListWith (++) $ [(c, group) | group <- cxtGroups, c <- group]
      cxt2Friends = Map.map Set.fromList cxt2Groups
      cxt2Contras = Map.map (\friends -> Set.difference allCxtSet friends) cxt2Friends
      addContradictors mite contras = mite { contradictors = Set.union (contradictors mite) contras }
      contras c fromMap = Map.findWithDefault Set.empty c fromMap
      createMite c = (mite c) { contradictors = Set.union (contras c cxt2ExistingContras) (contras c cxt2Contras)}
      newMites = map createMite allCxts
  in newMites

contradict mite1 mite2 = let
  allBaseMites m = m:(baseMites m >>= allBaseMites)
  allContradictors1 = foldl Set.union Set.empty (map contradictors $ allBaseMites mite1)
  in
  any (flip Set.member allContradictors1) $ map cxt $ allBaseMites mite2

hasContradictors mite inList = any (contradict mite) inList

withBase base mites = map (\m -> m {baseMites = LS.removeDups $ (baseMites m ++ base)}) mites
