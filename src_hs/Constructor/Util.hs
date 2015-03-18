module Constructor.Util where
import qualified Debug.Trace as DT
import Data.Char (toLower)
import Data.List
import Data.Function (on)

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

findDuplicate [] = Nothing
findDuplicate (x:xs) = if x `elem` xs then Just x else findDuplicate xs

isTrue m = m == Just True