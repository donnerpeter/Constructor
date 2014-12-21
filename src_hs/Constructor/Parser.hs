module Constructor.Parser where
import Constructor.Tree
import Constructor.ParsingState
import Constructor.Lexicon
import qualified Constructor.Sense as Sense
import Constructor.EnglishGenerator
import Constructor.Util
import Data.Char (toLower)
import Data.List

tokenize s = [map toLower token | token <- tokens s, length token > 0] where
  punctuation c = c == ':' || c == ',' || c == '.' || c == '\"' || c == '?'
  wordSymbol c = not (punctuation c) && c /= ' ' && c /= '\n'
  tokens s = case s of
    [] -> []
    c:cs -> case c of
      ' ' -> tokens cs
      '\n' -> let (token, rest) = span (== '\n') s in token : tokens rest
      _ | punctuation c -> [c]:tokens cs
      _ -> let (token, rest) = span wordSymbol s in token : tokens rest

parse:: String -> ParsingState
parse s =
  let tokens = tokenize s
      addWord state word = seq state $ {-trace word $ -}addMites state $ wordMites word (1 + length (history state))
      result = foldl' addWord emptyState tokens
  in result

translate s = generate $ makeSense s

makeSense s = resultSense $ roots $ parse s
resultSense trees = Sense.composeSense $ reverse $ map sense trees where
  emptySense = Sense.makeSense [] []