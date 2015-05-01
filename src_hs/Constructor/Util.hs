module Constructor.Util where
import qualified Debug.Trace as DT
import Data.Char (toLower, toUpper)
import Data.List
import Control.Monad

trace msg x = DT.traceShow msg x
traceIt msg x = trace (msg, x) x

isNumberString s = case reads s :: [(Int, String)] of
  [(_, "")] -> True
  _ -> False

leastValued f list = if length list <= 1 then list else filter (\e -> f e == best) list where
  best = minimum $ map f list

data Side = LeftSide | RightSide deriving (Eq, Show, Ord)

invert LeftSide = RightSide
invert RightSide = LeftSide
select LeftSide x _ = x
select RightSide _ x = x

decapitalize (c:cs) = (toLower c):cs
decapitalize [] = []

capitalize (c:rest) = (toUpper c):rest
capitalize [] = []

findDuplicate [] = Nothing
findDuplicate (x:xs) = if x `elem` xs then Just x else findDuplicate xs

isTrue m = m == Just True

cat "" t2 = t2
cat t1 "" = t1
cat t1 t2 = case t2 of
 [] -> t1
 c:_ -> if c `elem` ",.:;?!\n" then stripLastComma t1 ++ t2 else t1 ++ " " ++ t2

catM t1 t2 = liftM2 cat t1 t2

mapCat f list = foldM (\s arg -> return s `catM` f arg) "" list

stripLastComma t1 = if "," `isSuffixOf` t1 then take (length t1 - 1) t1 else t1
stripFirstComma t1 = if ", " `isPrefixOf` t1 then drop 2 t1 else t1
