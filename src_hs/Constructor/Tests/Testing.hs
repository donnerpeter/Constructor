module Constructor.Tests.Testing where

import Test.HUnit
import Constructor.EnglishGenerator
import Constructor.Parser
import Constructor.ParsingState
import Constructor.Sense as Sense
import Debug.Trace
import Control.DeepSeq
import Control.Monad (when)
import System.CPUTime
import System.IO.Unsafe

translateTest src target = textTranslateTest 100 src target

textTranslateTest time src target = TestLabel src $ TestCase $
  let trees = parse src
      sense = resultSense trees
      result = generate sense
      timed = unsafePerformIO $ do
        start <- getCPUTime
        finish <- result `deepseq` getCPUTime
        let ms = round $ fromIntegral (finish - start) / 1000 / 1000 / 1000
        when (ms > time) $ putStrLn $ show src ++ " took " ++ show ms
        return result
      message = if length src > 100 then ""  else show sense ++ "\n\n" ++ show trees
  in
  assertEqual message target timed
