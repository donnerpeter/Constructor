module Constructor.LinkedSet where
import qualified Data.Set as Set

data LinkedSet a = LinkedSet [a] (Set.Set a)

empty = LinkedSet [] Set.empty

add :: Ord a => a -> LinkedSet a -> LinkedSet a
add e set@(LinkedSet l s) = if Set.member e s then set else LinkedSet (e:l) (Set.insert e s)

addAll list set = foldl (\acc e -> add e acc) set list

elements (LinkedSet l _) = reverse l

member elem (LinkedSet l s) = Set.member elem s

fromList list = addAll list empty