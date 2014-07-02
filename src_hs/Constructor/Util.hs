module Constructor.Util where
import qualified Debug.Trace as DT
import Data.List
import Data.Function (on)

trace msg x = DT.traceShow msg x
traceIt msg x = trace (msg, x) x

isNumberString s = case reads s :: [(Int, String)] of
  [(_, "")] -> True
  _ -> False

leastValued f list = if length list <= 1 then list else takeWhile (\e -> f e == best) sorted where
  sorted = Data.List.sortBy (compare `on` f) list
  best = f $ head sorted

data Side = LeftSide | RightSide deriving (Eq, Show, Ord)

invert LeftSide = RightSide
invert RightSide = LeftSide
select LeftSide x _ = x
select RightSide _ x = x
