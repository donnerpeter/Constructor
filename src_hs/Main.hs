import Constructor.Tree
import Constructor.ParsingState
import Constructor.Lexicon
import Constructor.Parser
import Constructor.EnglishGenerator
import Constructor.Sense as Sense

import Test.HUnit
import Control.DeepSeq
import Control.Monad (when)
import System.CPUTime
import System.IO.Unsafe

import Constructor.Tests.Testing
import Constructor.Tests.Sonnet
import Constructor.Tests.SonnetVariations
import Constructor.Tests.Wh
import Constructor.Tests.RandomStuff
import Constructor.Tests.HybridCoordination
import Constructor.Tests.Copula
import Constructor.Tests.OldLadies
import Constructor.Tests.OldLadyVariations

createTest (TranslateTest src target time) = TestLabel src $ TestCase $ assertEqual message target timed where
  trees = parse src
  sense = resultSense trees
  result = generate sense
  timed = unsafePerformIO $ do
    start <- getCPUTime
    finish <- result `deepseq` getCPUTime
    let ms = round $ fromIntegral (finish - start) / 1000 / 1000 / 1000
    when (ms > time) $ putStrLn $ show src ++ " took " ++ show ms
    return result
  message = if length src > 100 then ""  else show sense ++ "\n\n" ++ show trees

allTests = runTestTT $ TestList $ map createTest $
  sonnetTests ++ sonnetVariations ++
  whTests ++ randomStuffTests ++ hybridCoordinationTests ++ copulaTests ++
  oldLadyTests ++ oldLadyVariationTests

main = allTests