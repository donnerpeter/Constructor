module Constructor.CopulaData where
import Constructor.Agreement
import Constructor.Variable

data CopulaKind = NPCopula | PPCopula | AdjCopula deriving (Show, Eq, Ord)

data CopulaData = CopulaData { copKind :: CopulaKind, copAgr :: Agr, copVar :: Variable, copBound :: Bool, copType :: String } deriving (Eq, Ord)
instance Show CopulaData where
  show cd = show (copKind cd) ++ " " ++ show (copAgr cd) ++ " " ++ (copType cd) ++ " " ++ show (copula cd) ++ (if copBound cd then " bound" else "")

copula cd = makeV (copVar cd) "cop" ""
copCP cd = makeV (copVar cd) "cop_cp" ""
copSubj cd = makeV (copVar cd) "cop_arg1" ""