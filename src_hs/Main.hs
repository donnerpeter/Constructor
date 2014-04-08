import Data.Char (toLower)

import Constructor.Tree
import Constructor.ParsingState
import Constructor.Lexicon
import Constructor.Parser
import Constructor.EnglishGenerator

import Constructor.Tests.Testing
import Constructor.Tests.Sonnet
import Constructor.Tests.SonnetVariations
import Constructor.Tests.Wh
import Constructor.Tests.RandomStuff
import Test.HUnit

allTests = runTestTT $ TestList $ sonnetTests++sonnetVariations++whTests++randomStuffTests

main = allTests
--main = putStrLn $ translate "Я забыл, что я забыл, что идет после 8 и что идет раньше 7"