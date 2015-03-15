import Constructor.Tree
import Constructor.ParsingState
import Constructor.Lexicon
import Constructor.Parser
import Constructor.EnglishGenerator
import qualified Constructor.Sense as Sense

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
  trees = roots $ parse src
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

demo = inner 0 where
  lines = sonnetSentences
  inner index = do
    _ <- getLine
    let sentence = lines !! index
    putStrLn $ "Translating: " ++ sentence
    putStrLn "..."
    putStrLn $ translate sentence
    putStrLn "------------------------"
    let nextIndex = index + 1
    if nextIndex < length lines then inner nextIndex
    else return ()