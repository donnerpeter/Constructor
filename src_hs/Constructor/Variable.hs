module Constructor.Variable where

import GHC.Generics (Generic)
import Data.Hashable

data Variable = Variable Int String deriving (Ord, Eq, Generic)
instance Show Variable where show (Variable i s) = "V"++(show i)++s
instance Hashable Variable

data SemValue = StrValue String | VarValue Variable deriving (Eq, Ord, Generic)
instance Show SemValue where
  show (StrValue s) = s
  show (VarValue v) = show v
instance Hashable SemValue