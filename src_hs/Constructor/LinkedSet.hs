module Constructor.LinkedSet where
import qualified Data.Set as Set

data LinkedSet a = LinkedSet [a] (Set.Set a) deriving (Show)

empty = LinkedSet [] Set.empty

add :: Ord a => a -> LinkedSet a -> LinkedSet a
add e set@(LinkedSet l s) = if Set.size newSet == Set.size s then set else LinkedSet (e:l) newSet
  where newSet = Set.insert e s

addAll list set = foldl (\acc e -> add e acc) set list

union set1 set2 = addAll (elements set2) set1

isEmpty (LinkedSet l _) = null l

elements (LinkedSet l _) = reverse l

member elem (LinkedSet l s) = Set.member elem s

fromList list = addAll list empty

removeDups list = elements $ fromList list