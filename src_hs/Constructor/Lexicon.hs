module Constructor.Lexicon where
import Constructor.Constructions
import Data.Char (ord, chr)

noun caze typ v = [mite $ Argument caze (v ""), semS (v "") "type" typ, mite $ AdjHead (v "") caze]
pronoun caze typ v = [mite $ Argument caze (v ""), semS (v "") "type" typ]
preposition prepArg nounArg v = [mite $ Argument prepArg (v ""), mite $ PrepHead nounArg (v "")]
finVerb typ time v = [semT (v "") typ, semS (v "") "time" time] ++ finiteClause v
finiteClause v = [mite $ NomHead (v "arg1"), semV (v "") "arg1" (v "arg1")] ++ clause v
clause v = [mite $ Verb (v "")] ++
                (xor [[mite $ TopLevelClause (v "cp")], [mite $ SubordinateClause (v "cp")]]) ++
                (xor [[mite $ Fact (v "cp"), semT (v "cp") "fact"], [mite $ Question (v "cp") (v ""), semT (v "cp") "question"]]) ++
                [semV (v "cp") "content" (v "")]
infinitive typ v = [semT (v "") typ, mite $ Infinitive $ v "", mite $ Verb (v "")]
arg argType relation v = [mite $ ArgHead argType (v relation), semV (v "") relation (v relation)]
whWord v = [mite $ Wh (v "") (v "cp"), mite $ QuestionVariants (Just $ v "") Nothing,  semT (v "") "wh"]
optional mites = xor [mites, [mite $ EmptyCxt $ cxt $ head mites]]
compHead attr v = [mite $ CompHead (v "comp"), semV (v "") attr (v "comp")] 

wordMites :: String -> Int -> [Mite]  
wordMites word index =
  let v = \i -> Variable index i
      v0 = v ""
  in
  case word of
  s | length (reads s :: [(Int, String)]) == 1 -> [mite $ Argument Nom v0, semT v0 word, semS v0 "number" "true"]
  "было" -> [mite $ CopulaTense v0, semS v0 "time" "PAST"]
  "вдруг" -> [mite $ Adverb "manner" "SUDDENLY"]
  "вспомнить" -> infinitive "RECALL" v ++ arg Acc "arg2" v
  "думают" -> finVerb "THINK" "PRESENT" v ++ arg Acc "arg2" v ++ arg PoDat "topic" v
  "забыл" -> finVerb "FORGET" "PAST" v ++ compHead "arg2" v
  "и" -> [mite $ Conjunction v0 "or", semS v0 "conj" "and", semT v0 "seq"]
  "идет" -> finVerb "COME_SCALARLY" "PRESENT" v ++ [mite $ ComeScalarly v0]
  "или" -> [mite $ Conjunction v0 "or", semS v0 "conj" "or", semT v0 "seq"]
  "их" -> xor [pronoun Acc "THEY" v, [semT v0 "THEY", mite $ Possessive Nom v0]]
  "к" -> preposition KDat Dat v
  -- todo wh-questions with каково
  "каково" -> finiteClause v ++ [mite $ Copula v0, semT (v "wh") "wh", semT v0 "degree", semV v0 "arg2" (v "wh"), mite $ ShortAdj (v "wh")]
  -- todo wh-questions with когда
  "когда" -> [mite $ ConditionComp v0 "when" False]
  "мной" -> pronoun Instr "ME" v
  "могут" -> finVerb "CAN" "PAST" v ++ [mite $ Control (v "theme"), semV v0 "theme" (v "theme")]
  "мое" -> [semT v0 "ME", mite $ Possessive Nom v0]
  "удивительный" -> [mite $ Adj v0 Nom "property" "AMAZING"]
  "обнаружили" -> finVerb "DISCOVER" "PAST" v ++ compHead "theme" v
  "он" -> pronoun Nom "HE" v
  "они" -> pronoun Nom "THEY" v
  "отправился" -> finVerb "GO_OFF" "PAST" v ++ arg KDat "goal" v
  "по" -> preposition PoDat Dat v
  "поводу" -> noun Dat "MATTER" v
  "порядок" -> noun Acc "ORDER" v ++ optional (arg Gen "arg1" v)
  "раньше" -> [mite $ ScalarAdverb "EARLIER" v0]
  "случай" -> noun Nom "THING" v
  "случился" -> finVerb "HAPPEN" "PAST" v ++ arg SInstr "experiencer" v
  "спросил" -> finVerb "ASK" "PAST" v ++ arg Acc "arg2" v ++ compHead "topic" v
  "со" -> preposition SInstr Instr v
  "соседям" -> noun Dat "NEIGHBORS" v
  "счета" -> noun Gen "COUNTING" v
  "удивление" -> noun Nom "AMAZE" v
  "что" -> whWord v ++ xor [[mite $ Argument Nom v0], [mite $ Argument Acc v0]]
  "этому" -> [mite $ Adj v0 Dat "determiner" "THIS"]
  "я" -> pronoun Nom "ME" v
  "-" -> [mite $ QuestionVariants Nothing (Just "-")]
  _ -> [mite $ Word v0 word]
