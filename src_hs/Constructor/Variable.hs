module Constructor.Variable where

data Variable = Variable Int String deriving (Ord, Eq)
instance Show Variable where show (Variable i s) = "V"++(show i)++s

data SemValue = StrValue String | VarValue Variable deriving (Eq, Ord)
instance Show SemValue where
  show (StrValue s) = s
  show (VarValue v) = show v
