module Constructor.Constructions where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Constructor.LinkedSet as LS
import Control.Exception (assert)
import Constructor.Agreement

data Variable = Variable Int String deriving (Ord, Eq)
instance Show Variable where show (Variable i s) = "V"++(show i)++s

data SemValue = StrValue String | VarValue Variable deriving (Eq, Ord)
instance Show SemValue where
  show (StrValue s) = s
  show (VarValue v) = show v

data ArgKind = Nom | Acc | Gen | Dat | Instr | Prep | PP String ArgKind | CP | PossKind ArgKind Agr | ScalarAdverb deriving (Show, Eq, Ord)
data Construction = Word Variable String
                  | Sem Variable String SemValue
                  | Unify Variable Variable
                  | Adj Variable ArgKind Agr String String
                  | AdjHead Variable ArgKind Agr
                  | Verb Variable
                  | NomHead Agr Variable
                  | ArgHead ArgKind Variable
                  | PrepHead ArgKind Variable
                  | Argument ArgKind Variable 
                  | Adverb String String
                  | Elaboration Variable
                  | CompHead Variable
                  | Wh Variable Variable
                  | QuestionVariants (Maybe Variable) (Maybe String)
                  | Conjunction Variable String
                  | SeqRight Variable ArgKind
                  | SeqFull Variable
                  | TopLevelClause Variable
                  | SubordinateClause Variable
                  | Fact Variable
                  | ElidedArgHead Construction
                  | Question Variable Variable
                  | Possessive ArgKind Agr Variable
                  | EmptyCxt Construction
                  | CopulaTense Variable
                  | Copula Variable
                  | ShortAdj Variable
                  | ConditionComp Variable {-if/when-} String {-has cp-} Bool
                  | CommaSurrounded {-closed-} Bool Construction
                  | Control Variable
                  | Infinitive Variable
                  | Complementizer Variable
                  | SurroundingComma {-closing-} Bool Variable
                  | Quote Variable {-closing-} Bool
                  | QuotedWord Construction {-closed-} Bool
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
isHappy (Argument {}) = False
isHappy (Elaboration {}) = False
isHappy (SeqRight {}) = False
isHappy (Conjunction {}) = False
isHappy (NomHead {}) = False
isHappy (ElidedArgHead {}) = False
isHappy (Possessive {}) = False
isHappy (CopulaTense {}) = False
isHappy (Copula {}) = False
isHappy (ConditionComp {}) = False
isHappy (CommaSurrounded {}) = False
isHappy (Infinitive {}) = False
isHappy (Control {}) = False
isHappy (SurroundingComma {}) = False
isHappy (SubordinateClause {}) = False
isHappy (QuotedWord _ False) = False
isHappy (Quote _ False) = False
isHappy _ = True

mite cxt = Mite cxt (isHappy cxt) Set.empty []
  
semS var prop value = mite $ Sem var prop (StrValue value)
semV var prop value = mite $ Sem var prop (VarValue value)
semT var _type = semS var "type" _type

xor :: [[Mite]] -> [Mite]
xor miteGroups =
  let cxtGroups = map (map cxt) miteGroups
      allCxts = LS.removeDups $ concat cxtGroups
      allCxtSet = Set.fromList $ concat cxtGroups
      cxt2Mites = Map.fromListWith (\v1 v2 -> assert (v1 == v2) v1) [(cxt mite, mite) | mite <- concat miteGroups]
      cxt2Groups = Map.fromListWith (++) $ [(c, group) | group <- cxtGroups, c <- group]
      cxt2Friends = Map.map Set.fromList cxt2Groups
      cxt2Contras = Map.map (\friends -> Set.difference allCxtSet friends) cxt2Friends
      addContradictors mite contras = mite { contradictors = Set.union (contradictors mite) contras }
      newMites = map (\c -> addContradictors ((Map.!) cxt2Mites c) (Map.findWithDefault Set.empty c cxt2Contras)) allCxts
  in newMites

contradict mite1 mite2 = Set.member (cxt mite1) (contradictors mite2) ||
                         any (contradict mite1) (baseMites mite2) ||
                         any (contradict mite2) (baseMites mite1)
hasContradictors mite inList = any (contradict mite) inList