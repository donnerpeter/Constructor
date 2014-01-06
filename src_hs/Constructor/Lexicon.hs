module Constructor.Lexicon where
import Constructor.Constructions
import Data.Char (ord, chr)

wordMites :: String -> Int -> [Mite]  
wordMites word index =
  let v = \i -> Variable index $ [chr (i+(ord 'a'))] in
  case word of
  "вдруг" -> [mite $ Adverb "manner" "SUDDENLY"]
  "забыл" -> [mite $ FiniteVerb (v 0), semS (v 0) "type" "FORGET", semS (v 0) "time" "PAST"]
  "мной" -> [mite $ Noun (v 0) Instr, mite $ Argument Instr (v 0), semT (v 0) "ME"]
  "удивительный" -> [mite $ Adj Nom "property" "AMAZING"]
  "случай" -> [mite $ Noun (v 0) Nom, semS (v 0) "type" "THING"]
  "случился" -> [mite $ FiniteVerb (v 0), mite $ ArgHead SInstr (v 1), semS (v 0) "type" "HAPPEN", semS (v 0) "time" "PAST", semV (v 0) "experiencer" (v 1)]
  "со" -> [mite $ Argument SInstr (v 0), mite $ ArgHead Instr (v 0)]
  "я" -> [mite $ Noun (v 0) Nom, semT (v 0) "ME"]
  _ -> [mite $ Word (v 0) word]
