module Constructor.LinkedHashSet where
import qualified Data.HashSet as Set
import Data.Hashable

data LinkedHashSet a = LinkedHashSet [a] (Set.HashSet a) deriving (Show)

empty = LinkedHashSet [] Set.empty

add :: (Ord a, Hashable a) => a -> LinkedHashSet a -> LinkedHashSet a
add e set@(LinkedHashSet l s) = if Set.size newSet == Set.size s then set else LinkedHashSet (e:l) newSet
  where newSet = Set.insert e s

addAll list set = foldl (\acc e -> add e acc) set list

elements (LinkedHashSet l _) = reverse l

member elem (LinkedHashSet l s) = Set.member elem s

fromList list = addAll list empty

removeDups list = elements $ fromList list