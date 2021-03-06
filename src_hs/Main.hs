{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}
import Constructor.Tree
import Constructor.ParsingState
import Constructor.Parser
import Constructor.EnglishGenerator

import Test.HUnit
import Control.DeepSeq
import Control.Monad (when)
import System.CPUTime
import System.Console.ANSI

import Constructor.Tests.Testing
import Constructor.Tests.Sonnet
import Constructor.Tests.SonnetVariations
import Constructor.Tests.Wh
import Constructor.Tests.RandomStuff
import Constructor.Tests.HybridCoordination
import Constructor.Tests.Copula
import Constructor.Tests.OldLadies

createTest (TranslateTest src target time) = TestLabel src $ TestCase check where
  trees = roots $ parse src
  sense = resultSense trees
  check = do
    (result, ms) <- timed $ generate sense
    when (ms > time) $
      putStrLn $ show src ++ " took " ++ show ms
    let message = "Expected: " ++ target ++
                "\nActual:   " ++ result ++
                (if length src > 100 then "" else "\n" ++ show sense ++ "\n\n" ++ show trees)
    when (target /= result) $
      assertFailure message

timed :: (NFData a) => a -> IO (a, Int)
timed value = do
  start <- getCPUTime
  finish <- value `deepseq` getCPUTime
  let ms = round $ fromIntegral (finish - start) / 1000 / 1000 / 1000
  return (value, ms)

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
    setSGR [SetColor Foreground Vivid Green]
    putStrLn sentence
    setSGR []
    putStrLn "translating..."
    setSGR [SetColor Foreground Vivid Blue]
    putStrLn $ translate sentence
    setSGR []
    putStrLn "------------------------"
    let nextIndex = index + 1
    if nextIndex < length lines then inner nextIndex
    else return ()