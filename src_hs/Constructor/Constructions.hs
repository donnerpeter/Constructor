module Constructor.Constructions where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Constructor.LinkedSet as LS

data Variable = Variable Int String deriving (Ord, Eq)
instance Show Variable where show (Variable i s) = "V"++(show i)++s

data SemValue = StrValue String | VarValue Variable deriving (Eq, Ord)
instance Show SemValue where
  show (StrValue s) = s
  show (VarValue v) = show v

data ArgKind = Nom | Acc | Gen | Dat | Instr | Prep | SInstr | KDat | PoDat deriving (Show, Eq, Ord)
data Construction = Word Variable String
                  | Sem Variable String SemValue
                  | Unify Variable Variable
                  | Adj Variable ArgKind String String
                  | AdjHead Variable ArgKind
                  | Noun Variable ArgKind
                  | FiniteVerb Variable
                  | ArgHead ArgKind Variable
                  | Argument ArgKind Variable 
                  | Adverb String String
                  | Elaboration Variable
                  | ComeScalarly Variable
                  | CompHead Variable
                  | CompComma Variable
                  | ScalarAdverb String Variable
                  | Wh Variable Variable
                  | QuestionVariants (Maybe Variable) (Maybe String)
                  | Conjunction Variable String
                  | SeqRight Variable
                  | SeqFull Variable
                  deriving (Show, Ord, Eq)
data Mite = Mite { cxt :: Construction, happy :: Bool, contradictors :: Set.Set Construction } deriving (Ord, Eq)
instance Show Mite where
  show (Mite {cxt=c, happy=h, contradictors=cc}) =
    (if h then "" else "!") ++ (show c) ++ (if Set.null cc then "" else "(xor "++(show cc)++")")
  
isHappy (Noun {}) = False
isHappy (Adj {}) = False
isHappy (Adverb {}) = False
isHappy (ArgHead {}) = False
isHappy (Argument {}) = False
isHappy (Elaboration {}) = False
isHappy _ = True

mite cxt = Mite cxt (isHappy cxt) Set.empty
  
semS var prop value = mite $ Sem var prop (StrValue value)
semV var prop value = mite $ Sem var prop (VarValue value)
semT var _type = semS var "type" _type

xor :: [[Mite]] -> [Mite]
xor miteGroups =
  let cxtGroups = map (map cxt) miteGroups
      allCxts = LS.elements $ LS.fromList $ concat cxtGroups
      allCxtSet = Set.fromList $ concat cxtGroups
      cxt2Groups = Map.fromListWith (++) $ concat [[(c, group) | c <- group] | group <- cxtGroups]
      cxt2Friends = Map.map Set.fromList cxt2Groups
      cxt2Contras = Map.map (\friends -> Set.difference allCxtSet friends) cxt2Friends
      newMites = map (\c -> Mite c (isHappy c) $ Map.findWithDefault Set.empty c cxt2Contras) allCxts
  in newMites