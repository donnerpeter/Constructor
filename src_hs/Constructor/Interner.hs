module Constructor.Interner (Interned(..), Interner, emptyInterner, intern, internerSize) where
import qualified Data.Map as Map
import Constructor.Util

data (Ord key) => Interner key value = Interner { enumMap :: Map.Map key Int, enumValues :: [value] }
data Interned value = Interned { internedKey :: Int, internedValue :: value}

intern :: (Ord key, Show key) => Interner key value -> key -> value -> (Interned value, Interner key value)
intern e@(Interner enumMap enumValues) key value = case Map.lookup key enumMap of
  Just i -> {-trace ("cached", key) $ -}(Interned i (enumValues !! i), e)
  _ -> {-trace ("non-cached", key) $ -}let i = internerSize e in
    (Interned i value, Interner (Map.insert key i enumMap) (enumValues ++ [value]))

emptyInterner :: (Ord key) => Interner key value
emptyInterner = Interner Map.empty []

internerSize i = Map.size $ enumMap i