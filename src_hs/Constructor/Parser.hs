module Constructor.Parser where
import Constructor.Tree
import Constructor.ParsingState
import Constructor.Lexicon
import qualified Constructor.Sense as Sense
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
      addWord state word = seq state $ {-trace word $ -}addMites state $ wordMites word (1 + length (history state))
      result = foldl' addWord emptyState tokens
  in roots result

translate s = generate $ makeSense s

makeSense s = Sense.makeSense $ activeStateMites $ parse s