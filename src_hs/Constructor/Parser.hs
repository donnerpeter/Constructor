module Constructor.Parser where
import Constructor.Tree
import Constructor.ParsingState
import Constructor.Lexicon
import Constructor.Sense
import Constructor.EnglishGenerator
import Constructor.Util
import Data.Char (toLower)
import Data.List

tokenize s = [map toLower token | token <- tokens, length token > 0] where
  tokens = reverse $ fst $ foldl processChar ([], "") (s++" ")
  processChar = \(tokens, current) char ->
    case char of
      ' ' -> (current:tokens, "")
      '\n' -> (current:tokens, "")
      c | c == ':' || c == ',' || c == '.' || c == '\"' || c == '?' -> ([char]:current:tokens, "")
      _ -> (tokens, current++[char])

parse:: String -> [Tree]
parse s =
  let tokens = tokenize s
      pair = foldl' (\(state, index) word -> seq state $ {-trace word $ -}(addMites state (wordMites word index), index + 1)) ([], 1) tokens
  in fst pair

translate s = generate $ makeSense $ parse s
