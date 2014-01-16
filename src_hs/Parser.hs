import Data.Char (toLower)
import Constructor.Tree
import Constructor.Lexicon
import Constructor.Sense
import Constructor.EnglishGenerator
import Test.HUnit

tokenize s = map (\x -> map toLower x) $
             filter (\token -> length token > 0) $
             reverse $
             fst $
             foldl processChar ([], "") (s++" ") where
  processChar = \(tokens, current) char ->
    case char of
      ' ' -> (current:tokens, "")
      ':' -> (":":current:tokens, "")
      ',' -> (",":current:tokens, "")
      _ -> (tokens, current++[char])

parse:: String -> [Tree]
parse s =
  let tokens = tokenize s
      pair = foldl (\(state, index) word -> (addMites state (wordMites word index), index + 1)) ([], 1) tokens
  in fst pair

translate s = generate $ makeSense $ parse s

translateTest src target = TestLabel src $ TestCase $
  let trees = parse src in
  assertEqual ((show $ makeSense trees) ++ "\n\n" ++ (show trees)) target (translate src) 

sonnetTests = [
  translateTest "Удивительный случай случился со мной: я вдруг забыл, что идет раньше - 7 или 8" 
          "An amazing thing happened to me today, I suddenly forgot what comes first - 7 or 8"
  ]

sonnetVariationTests=[
--  translateTest "Удивительный случай случился со мной: я вдруг забыл, что идет раньше - 7 или 8" 
--          "An amazing thing happened to me today, I suddenly forgot what comes first - 7 or 8",
--  translateTest "Я вдруг забыл, что - 7 или 8 - идет раньше" 
--         "I suddenly forgot what comes first - 7 or 8",
--  translateTest "Я вдруг забыл, что, 7 или 8, идет раньше" 
--         "I suddenly forgot what comes first - 7 or 8"
  ]

tests = TestList (sonnetTests++sonnetVariationTests)

allTests = runTestTT tests