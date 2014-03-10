module Constructor.Constructions where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Constructor.LinkedSet as LS
import Control.Exception (assert)
import Constructor.Agreement
import Constructor.Variable
import Data.Maybe

data ArgKind = Nom | Acc | Gen | Dat | Instr | Prep | PP String ArgKind | 
               ClauseArg ClauseForce | CP | PossKind ArgKind Agr | ScalarAdverb
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
                  | Conjunction SeqData
                  | Clause ClauseForce Variable
                  | ElidedArgHead Construction
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
                  | Sentence Variable
                  | ConjEmphasis String Variable
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
  
isHappy cxt = case cxt of
  Adj {} -> False; Adverb {} -> False
  ArgHead {} -> False; PrepHead {} -> False; Argument {} -> False; ElidedArgHead {} -> False
  CompHead {} -> False; ConditionCompHead {} -> False; ConditionComp {} -> False; ReasonComp {} -> False
  Elaboration {} -> False
  Conjunction (SeqData {seqHasLeft = seqHasLeft, seqHasRight = seqHasRight}) -> seqHasLeft && seqHasRight
  NomHead {} -> False; GenHead {} -> False; Possessive {} -> False
  CopulaTense {} -> False; Copula {} -> False
  CommaSurrounded {} -> False; SurroundingComma {} -> False
  ControlledInfinitive {} -> False; Control {} -> False
  Clause {} -> False
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