module Constructor.Lexicon where
import Constructor.Constructions
import Data.Char (ord, chr)

noun caze typ v = [mite $ Noun (v "") caze, semS (v "") "type" typ, mite $ AdjHead (v "") caze, mite $ Argument caze (v "")]
pronoun caze typ v = [mite $ Noun (v "") caze, semS (v "") "type" typ, mite $ Argument caze (v "")]
preposition prepArg nounArg v = [mite $ Argument prepArg (v ""), mite $ ArgHead nounArg (v "")]
finVerb typ time v = [mite $ FiniteVerb (v ""), semT (v "") typ, semS (v "") "time" time] ++
  (xor [[mite $ TopLevelClause (v "cp")], [mite $ SubordinateClause (v "cp")]]) ++
  (xor [[mite $ Fact (v "cp"), semT (v "cp") "fact"], [mite $ Question (v "cp") (v ""), semT (v "cp") "question"]]) ++
  [semV (v "cp") "content" (v "")]
arg argType relation v = [mite $ ArgHead argType (v relation), semV (v "") relation (v relation)]

wordMites :: String -> Int -> [Mite]  
wordMites word index =
  let v = \i -> Variable index i
      v0 = v ""
  in
  case word of
  s | length (reads s :: [(Int, String)]) == 1 -> [mite $ Noun v0 Nom, semT v0 word, semS v0 "number" "true"]
  "вдруг" -> [mite $ Adverb "manner" "SUDDENLY"]
  "думают" -> (finVerb "THINK" "PRESENT" v) ++ (arg Acc "arg2" v) ++ (arg PoDat "topic" v)
  "забыл" -> (finVerb "FORGET" "PAST" v) ++ [mite $ CompHead (v "comp"), semV v0 "arg2" (v "comp")]
  "идет" -> (finVerb "COME_SCALARLY" "PRESENT" v) ++ [mite $ ComeScalarly v0]
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
  "спросил" -> (finVerb "ASK" "PAST" v) ++ (arg Acc "arg2" v) ++ [mite $ CompHead (v "comp"), semV v0 "topic" (v "comp")]
  "со" -> preposition SInstr Instr v
  "соседям" -> noun Dat "NEIGHBORS" v
  "что" -> [mite $ Wh v0 (v "cp"), mite $ QuestionVariants (Just v0) Nothing,  semT v0 "WH", mite $ Noun v0 Nom]
  "этому" -> [mite $ Adj v0 Dat "determiner" "THIS"]
  "я" -> pronoun Nom "ME" v
  "-" -> [mite $ QuestionVariants Nothing (Just "-")]
  _ -> [mite $ Word v0 word]
