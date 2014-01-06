import Data.Char (toLower)
import Constructor.Tree
import Constructor.Lexicon
import Constructor.Sense

tokenize s = map (\x -> map toLower x) $
             filter (\token -> length token > 0) $
             reverse $
             fst $
             foldl processChar ([], "") (s++" ") where
  processChar = \(tokens, current) char ->
    case char of
      ' ' -> (current:tokens, "")
      ':' -> (":":current:tokens, "")
      _ -> (tokens, current++[char])

parse:: String -> [Tree]
parse s =
  let tokens = tokenize s
      pair = foldl (\(state, index) word -> (addMites state (wordMites word index), index + 1)) ([], 1) tokens
  in fst pair