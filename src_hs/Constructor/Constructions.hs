module Constructor.Constructions where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Constructor.LinkedSet as LS
import Control.Exception (assert)
import Constructor.Agreement
import Constructor.Variable
import Debug.Trace

data ArgKind = Nom | Acc | Gen | Dat | Instr | Prep | PP String ArgKind | 
               ClauseArg ClauseLevel | CP | PossKind ArgKind Agr | ScalarAdverb
               deriving (Show, Eq, Ord)
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
                  | TwoWordCxt String {-first-} Bool [Mite] Variable
                  | ReasonComp Variable {-has cp-} Bool
                  | ReflexiveReference Variable
                  | ReflexiveTarget Variable
                  -- | S1 | S2 | S3 | S4
                  deriving (Show, Ord, Eq)
type XorKey = (Construction, [Mite])
data Mite = Mite { cxt :: Construction, happy :: Bool, contradictors :: Set.Set XorKey, baseMites :: [Mite] }
instance Show Mite where
  show (Mite {cxt=c, happy=h, contradictors=cc, baseMites = b}) =
    (if h then "" else "!") ++ show c -- ++ (if Set.null cc then "" else "(xor "++(show cc)++")")
        -- ++ (if null b then "" else "(base " ++ show b ++ "/base)")
instance Ord Mite where compare m1 m2 = compare (xorKey m1) (xorKey m2)
instance Eq Mite where m1 == m2 = (xorKey m1) == (xorKey m2)
  
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
isHappy (Complement {}) = False
isHappy (Wh {}) = False
isHappy (RelativeClause {}) = False
isHappy _ = True

isCommaSurroundable (ConditionComp _ _ True) = True
isCommaSurroundable (ReasonComp _ True) = True
isCommaSurroundable (Complement {}) = True
isCommaSurroundable (RelativeClause {}) = True
isCommaSurroundable (VerbalModifier _ True _) = True
isCommaSurroundable _ = False

mite cxt = Mite cxt (isHappy cxt) Set.empty []
  
semS var prop value = mite $ Sem var prop (StrValue value)
semV var prop value = mite $ Sem var prop (VarValue value)
semT var _type = semS var "type" _type

xorKey mite = (cxt mite, baseMites mite)

xor :: [[Mite]] -> [Mite]
xor miteGroups =
  let cxtGroups = map (map xorKey) miteGroups
      allCxts = LS.removeDups $ concat cxtGroups
      allCxtSet = Set.fromList allCxts

      cxt2ExistingContras :: Map.Map XorKey (Set.Set XorKey)
      cxt2ExistingContras = Map.fromListWith Set.union [(xorKey mite, contradictors mite) | mite <- concat miteGroups]
      cxt2Friends = Map.fromListWith Set.union $ [(c, Set.fromList group) | group <- cxtGroups, c <- group]
      cxt2Contras = Map.map (\friends -> Set.difference allCxtSet friends) cxt2Friends

      contras :: XorKey -> Map.Map XorKey (Set.Set XorKey) -> Set.Set XorKey
      contras c fromMap = Map.findWithDefault Set.empty c fromMap
      createMite key@(c, b) = (mite c) { contradictors = Set.union (contras key cxt2ExistingContras) (contras key cxt2Contras), baseMites = b}
      newMites = map createMite allCxts
  in assert (LS.removeDups newMites == newMites) $ {-traceShow ("xor", miteGroups) $ traceShow ("->", newMites) $ -}newMites
  
flattenBaseMites m = m:(baseMites m >>= flattenBaseMites)
flattenContradictors mite = foldl Set.union Set.empty (map contradictors $ flattenBaseMites mite) where

contradict mite1 mite2 = let allContradictors1 = flattenContradictors mite1 in
  any (flip Set.member allContradictors1) $ map xorKey $ flattenBaseMites mite2

buildContradictorCache mites = Map.fromList [(m, Set.fromList $ findContradictors t) | t@(m, _, _) <- triples] where
  triples = [(m, LS.removeDups $ map xorKey $ flattenBaseMites m, flattenContradictors m) | m <- mites]
  findContradictors (mite, _, allContradictors) = [m | (m, _, _) <- filter contradicts triples] where
    contradicts (m2, flatBase2, _) = any (flip Set.member allContradictors) flatBase2

withBase base mites = let
  keys = Set.fromList $ map xorKey mites
  addBaseToContra key@(c, b) = if Set.member key keys then (c, LS.removeDups $ b ++ base) else key
  in
  map (\m -> m {baseMites = LS.removeDups $ (baseMites m ++ base), contradictors = Set.map addBaseToContra $ contradictors m }) mites

optional mites = xor [mites, [mite $ EmptyCxt $ cxt $ head mites]]