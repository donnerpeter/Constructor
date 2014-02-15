import Data.Char (toLower)

import Constructor.Tree
import Constructor.ParsingState
import Constructor.Lexicon
import Constructor.Sense
import Constructor.Parser
import Constructor.EnglishGenerator

import Constructor.Tests.Testing
import Constructor.Tests.Sonnet
import Constructor.Tests.SonnetVariations
import Constructor.Tests.Wh
import Constructor.Tests.RandomStuff
import Test.HUnit

allTests = runTestTT $ TestList $ sonnetTests++sonnetVariations++whTests++randomStuffTests

main :: IO Counts
main = allTests