module Constructor.CopulaData where
import Constructor.Agreement
import Constructor.Variable

data CopulaKind = NPCopula | PPCopula | AdjCopula deriving (Show, Eq, Ord)

data CopulaData = CopulaData { copKind :: CopulaKind, copAgr :: Agr, copSubj :: Variable, copula :: Variable, copCP :: Variable, copBound :: Bool, copType :: String } deriving (Eq, Ord)
instance Show CopulaData where
  show cd = show (copKind cd) ++ " " ++ show (copAgr cd) ++ " " ++ (copType cd) ++ " " ++ show (copula cd) ++ (if copBound cd then " bound" else "")
