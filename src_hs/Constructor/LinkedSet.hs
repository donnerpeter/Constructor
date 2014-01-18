module Constructor.LinkedSet where
import qualified Data.Set as Set

data LinkedSet a = LinkedSet [a] (Set.Set a)
empty = LinkedSet [] Set.empty
addAll list (LinkedSet l s) =
  let reallyNew = [e | e <- list, not $ Set.member e s]
  in LinkedSet (l++reallyNew) (Set.union s $ Set.fromList reallyNew)
elements (LinkedSet l _) = reverse l
member elem (LinkedSet l s) = Set.member elem s