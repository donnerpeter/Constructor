import Data.Char (toLower)
import Constructor.Tree
import Constructor.ParsingState
import Constructor.Lexicon
import Constructor.Sense
import Constructor.EnglishGenerator
import Test.HUnit
import Debug.Trace

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
      '.' -> (".":current:tokens, "")
      _ -> (tokens, current++[char])

parse:: String -> [Tree]
parse s =
  let tokens = tokenize s
      pair = foldl (\(state, index) word -> (addMites state (wordMites word index), index + 1)) ([], 1) tokens
  in fst pair

translate s = generate $ makeSense $ parse s

translateTest src target = TestLabel src $ TestCase $
  let trees = parse src in
  assertEqual ((show $ makeSense trees) ++ "\n\n" ++ (show trees)) target (generate $ makeSense trees) 

sonnetTests = [
  translateTest "Удивительный случай случился со мной: я вдруг забыл, что идет раньше - 7 или 8" 
                "An amazing thing happened to me today, I suddenly forgot what comes first - 7 or 8",
  translateTest "Я отправился к соседям и спросил их, что они думают по этому поводу" 
                "I went to my neighbors and asked them about their opinion on this matter"
                ,
  translateTest "Каково же было их и мое удивление, когда они вдруг обнаружили, что тоже не могут вспомнить порядок счета." 
                "Great was their and my amazement, when they suddenly discovered, that they couldn't recall the counting order."
                ,
  translateTest "1, 2, 3, 4, 5 и 6 помнят, а дальше забыли." 
                "They remember 1, 2, 3, 4, 5 and 6, but forgot what comes next."
                ,
  translateTest "Каково же было их и мое удивление, когда они вдруг обнаружили, что тоже не могут вспомнить порядок счета. \
                        \1, 2, 3, 4, 5 и 6 помнят, а дальше забыли."
                "Great was their and my amazement, when they suddenly discovered, that they couldn't recall the counting order. \
                        \They remembered 1, 2, 3, 4, 5 and 6, but forgot what comes next."
  ]

variationTests1=[
--  translateTest "Я вдруг забыл, что идет дальше." 
--          "I suddenly forgot what comes next."
--  ,
--  translateTest "Я вдруг забыл, что - 7 или 8 - идет раньше" "I suddenly forgot what comes first - 7 or 8"
--  ,
--  translateTest "Я вдруг забыл, что, 7 или 8, идет раньше" "I suddenly forgot what comes first - 7 or 8"
--  ,
--  translateTest "Я вдруг забыл, кого я видел - кассиршу или соседей" 
--         "I suddenly forgot who I saw, the cashier or my neighbors"
--  ,
--  translateTest "Я забыл, что идёт раньше - 7, 8 или 9." "I forgot what comes first - 7, 8 or 9."
--  ,
--  translateTest "Я забыл, что 7 идёт раньше 8." "I forgot, that a 7 comes before an 8."
--  ,
--  translateTest "Я забыл, что 7 идёт раньше 8 и что 4 идёт после 3."
--                "I forgot, that a 7 comes before an 8 and that a 4 comes after a 3."
--  ,
--  translateTest "Я забыл, что я это забыл" "I forgot, that I forgot that"
--  ,
--  translateTest "Я забыл, что я забыл это" "I forgot, that I forgot that"
--  ,
--  translateTest "Я забыл, что я забыл" "I forgot what I forgot"
--  ,
--  translateTest "Я забыл, что я забыл, что 7 идет раньше 8" "I forgot, that I forgot, that a 7 comes before an 8"
--  ,
--  translateTest "Я забыл, что я забыл, что идет после 8" "I forgot, that I forgot what comes after an 8"
--  ,
--  translateTest "Я забыл, что я делал."
--                    "I forgot what I did."
--  ,
--  translateTest "Я забыл, что я делал, что идет после 8 и что идет раньше 7."
--                    "I forgot what I did, what comes after an 8, and what comes before a 7."
--  ,
--  translateTest "Я забыл, что я забыл, что идет после 8 и что идет раньше 7"
--                    "I forgot, that I forgot what comes after an 8 and what comes before a 7"
--  ,
--  translateTest "Я забыл, что идёт после 7 и что идёт раньше 8." "I forgot what comes after a 7 and what comes before an 8."
--  ,
--  translateTest "Я забыл, что 8 идёт после 7 и что идёт раньше 5."
--            "I forgot, that an 8 comes after a 7 and what comes before a 5."
--  ,
--  translateTest "Я забыл, что идёт после 7 и что 4 идёт раньше 5."
--            "I forgot what comes after a 7 and that a 4 comes before a 5."
  ]
variationTests2=[
  translateTest "Он отправился к соседям" 
                "He went to his neighbors"
  ,
  translateTest "Я спросил их, что они думают по этому поводу" 
                "I asked them about their opinion on this matter"
  ]
tests = TestList (sonnetTests++variationTests1++variationTests2)

allTests = runTestTT tests

main :: IO Counts
main = allTests