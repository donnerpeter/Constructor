module Constructor.Lexicon where
import Constructor.Constructions
import Data.Char (ord, chr)

readsInt :: String -> [(Int, String)]
readsInt = reads

noun caze typ v = [mite $ Noun (v 0) caze, semS (v 0) "type" typ, mite $ AdjHead (v 0) caze, mite $ Argument caze (v 0)]
pronoun caze typ v = [mite $ Noun (v 0) caze, semS (v 0) "type" typ, mite $ Argument caze (v 0)]
preposition prepArg nounArg v = [mite $ Argument prepArg (v 0), mite $ ArgHead nounArg (v 0)]
finVerb typ time v = [mite $ FiniteVerb (v 0), semT (v 0) typ, semS (v 0) "time" time]
arg argType relation v = [mite $ ArgHead argType (v 1), semV (v 0) relation (v 1)]

wordMites :: String -> Int -> [Mite]  
wordMites word index =
  let v = \i -> Variable index $ [chr (i+(ord 'a'))]
      v0 = v 0
  in
  case word of
  s | length (readsInt s) == 1 -> [mite $ Noun v0 Nom, semT v0 word, semS v0 "number" "true"]
  "вдруг" -> [mite $ Adverb "manner" "SUDDENLY"]
  "думают" -> (finVerb "THINK" "PRESENT" v) ++ (arg Acc "arg2" v) ++ (arg PoDat "topic" v)
  "забыл" -> (finVerb "FORGET" "PAST" v) ++ [mite $ CompHead (v 1), semV v0 "arg2" (v 1)]
  "идет" -> (finVerb "COME_SCALARLY" "PRESENT" v) ++ [mite $ ComeScalarly (v 0)]
  "или" -> [mite $ Conjunction v0 "or", semS v0 "conj" "or", semT v0 "SEQ"]
  "их" -> pronoun Acc "THEY" v
  "к" -> preposition KDat Dat v
  "мной" -> pronoun Instr "ME" v
  "удивительный" -> [mite $ Adj v0 Nom "property" "AMAZING"]
  "он" -> pronoun Nom "HE" v
  "они" -> pronoun Nom "THEY" v
  "отправился" -> (finVerb "GO_OFF" "PAST" v) ++ (arg KDat "goal" v)
  "по" -> preposition PoDat Dat v
  "поводу" -> noun Dat "MATTER" v
  "раньше" -> [mite $ ScalarAdverb "EARLIER" v0]
  "случай" -> noun Nom "THING" v
  "случился" -> (finVerb "HAPPEN" "PAST" v) ++ (arg SInstr "experiencer" v)
  "спросил" -> (finVerb "ASK" "PAST" v) ++ (arg Acc "arg2" v) ++ [mite $ CompHead (v 2), semV v0 "topic" (v 2)]
  "со" -> preposition SInstr Instr v
  "соседям" -> noun Dat "NEIGHBORS" v
  "что" -> [mite $ Wh v0 (v 1), mite $ QuestionVariants (Just v0) Nothing,  semT v0 "WH", semT (v 1) "question", semV (v 1) "questioned" v0, mite $ Noun (v 0) Nom]
  "этому" -> [mite $ Adj v0 Dat "determiner" "THIS"]
  "я" -> pronoun Nom "ME" v
  "-" -> [mite $ QuestionVariants Nothing (Just "-")]
  _ -> [mite $ Word (v 0) word]
