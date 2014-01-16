module Constructor.Lexicon where
import Constructor.Constructions
import Data.Char (ord, chr)

readsInt :: String -> [(Int, String)]
readsInt = reads

wordMites :: String -> Int -> [Mite]  
wordMites word index =
  let v = \i -> Variable index $ [chr (i+(ord 'a'))]
      v0 = v 0
  in
  case word of
  s | length (readsInt s) == 1 -> [mite $ Noun v0 Nom, semT v0 word, semS v0 "number" "true"]
  "вдруг" -> [mite $ Adverb "manner" "SUDDENLY"]
  "забыл" -> [mite $ FiniteVerb (v 0), mite $ CompHead (v 1), semS (v 0) "type" "FORGET", semS (v 0) "time" "PAST", semV (v 0) "arg2" (v 1)]
  "идет" -> [mite $ FiniteVerb (v 0), mite $ ComeScalarly (v 0), semS (v 0) "time" "PRESENT"]
  "или" -> [mite $ Conjunction v0 "or", semS v0 "conj" "or"]
  "мной" -> [mite $ Noun (v 0) Instr, mite $ Argument Instr (v 0), semT (v 0) "ME"]
  "удивительный" -> [mite $ Adj v0 Nom "property" "AMAZING"]
  "раньше" -> [mite $ ScalarAdverb "EARLIER" v0]
  "случай" -> [mite $ Noun (v 0) Nom, semS (v 0) "type" "THING", mite $ AdjHead v0 Nom]
  "случился" -> [mite $ FiniteVerb (v 0), mite $ ArgHead SInstr (v 1), semS (v 0) "type" "HAPPEN", semS (v 0) "time" "PAST", semV (v 0) "experiencer" (v 1)]
  "со" -> [mite $ Argument SInstr (v 0), mite $ ArgHead Instr (v 0)]
  "что" -> [mite $ Wh v0 (v 1), mite $ QuestionVariants (Just v0) Nothing,  semT v0 "WH", semT (v 1) "question", semV (v 1) "questioned" v0, mite $ Noun (v 0) Nom]
  "я" -> [mite $ Noun (v 0) Nom, semT (v 0) "ME"]
  "-" -> [mite $ QuestionVariants Nothing (Just "-")]
  _ -> [mite $ Word (v 0) word]
