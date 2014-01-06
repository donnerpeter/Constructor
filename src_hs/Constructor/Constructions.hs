module Constructor.Constructions where

data Variable = Variable Int String deriving (Ord, Eq)
instance Show Variable where show (Variable i s) = "V"++(show i)++s

data SemValue = StrValue String | VarValue Variable
instance Show SemValue where
  show (StrValue s) = s
  show (VarValue v) = show v

data ArgKind = Nom | Acc | Gen | Dat | Instr | Prep | SInstr deriving (Show, Eq)
data Construction = Word Variable String
                  | Sem Variable String SemValue
                  | Unify Variable Variable
                  | Adj ArgKind String String
                  | Noun Variable ArgKind
                  | FiniteVerb Variable
                  | ArgHead ArgKind Variable
                  | Argument ArgKind Variable 
                  | Adverb String String
                  | Elaboration Variable
                  deriving (Show)
data Mite = Mite { cxt :: Construction, happy :: Bool }
instance Show Mite where
  show (Mite {cxt=c, happy=h}) = (if h then "" else "!")++(show c)
  
isHappy (Noun {}) = False
isHappy (Adj {}) = False
isHappy (Adverb {}) = False
isHappy (ArgHead {}) = False
isHappy (Argument {}) = False
isHappy _ = True

mite cxt = Mite cxt $ isHappy cxt
  
semS var prop value = mite $ Sem var prop (StrValue value)
semV var prop value = mite $ Sem var prop (VarValue value)
semT var _type = semS var "type" _type