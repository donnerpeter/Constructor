module Constructor.Interner (Interned(..), Interner, emptyInterner, intern, internerSize) where
import qualified Data.Map as Map

data (Ord key) => Interner key value = Interner { enumMap :: Map.Map key Int, enumValues :: [value] }
data Interned value = Interned { internedKey :: Int, internedValue :: value}

intern :: (Ord key, Show key) => Interner key value -> key -> value -> (Interned value, Interner key value)
intern e key value = case Map.lookup key (enumMap e) of
  Just i -> (Interned i (enumValues e !! i), e)
  _ -> let i = internerSize e in
    (Interned i value, Interner (Map.insert key i $ enumMap e) (enumValues e ++ [value]))

emptyInterner :: (Ord key) => Interner key value
emptyInterner = Interner Map.empty []

internerSize i = Map.size $ enumMap i