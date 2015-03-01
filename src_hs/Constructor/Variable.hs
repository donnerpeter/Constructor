module Constructor.Variable where

import GHC.Generics (Generic)
import Data.Hashable
import Constructor.SemanticProperties
import Constructor.Util

data Variable = Variable Int String deriving (Ord, Eq, Generic)
instance Show Variable where show (Variable i s) = "V"++(show i)++s
instance Hashable Variable

data SemValue = StrValue StrProperty String | VarValue VarProperty Variable deriving (Eq, Ord, Generic)
instance Show SemValue where
  show (StrValue p s) = (decapitalize $ show p) ++ "=" ++ s
  show (VarValue p v) = (decapitalize $ show p) ++ "=" ++ show v
instance Hashable SemValue

modifyV v c = \s -> v $ c ++ s
makeV (Variable index oldS) suffix = \s -> Variable index (oldS ++ suffix ++ s)
