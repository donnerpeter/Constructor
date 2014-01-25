module Constructor.Lexicon where
import Constructor.Constructions
import qualified Constructor.Agreement as A
import Constructor.Agreement (Gender(..))
import Data.Char (ord, chr)

nounSg caze gender typ v = pronoun caze (A.Agr (Just gender) A.Sg (Just 3)) typ v
nounPl caze typ v = pronoun caze (A.Agr Nothing A.Pl (Just 3)) typ v 
pronoun caze agr typ v = [mite $ Argument caze (v ""), semS (v "") "type" typ, mite $ AdjHead (v "") caze agr]
preposition prepArg nounArg v = [mite $ Argument prepArg (v ""), mite $ PrepHead nounArg (v "")]
finVerb typ time agr v = [semT (v "") typ, semS (v "") "time" time] ++ finiteClause agr v
finiteClause agr v = [mite $ NomHead agr (v "arg1"), semV (v "") "arg1" (v "arg1")] ++ clause v
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
  "думают" -> finVerb "THINK" "PRESENT" A.pl3 v ++ arg Acc "arg2" v ++ arg PoDat "topic" v
  "забыл" -> finVerb "FORGET" "PAST" A.m v ++ compHead "arg2" v
  "и" -> [mite $ Conjunction v0 "or", semS v0 "conj" "and", semT v0 "seq"]
  "идет" -> finVerb "COME_SCALARLY" "PRESENT" A.sg3 v ++ [mite $ ComeScalarly v0]
  "или" -> [mite $ Conjunction v0 "or", semS v0 "conj" "or", semT v0 "seq"]
  "их" -> xor [pronoun Acc A.pl "THEY" v, [semT v0 "THEY", mite $ Possessive Nom A.sg v0], [semT v0 "THEY", mite $ Possessive Nom A.pl v0]]
  "к" -> preposition KDat Dat v
  -- todo wh-questions with каково
  "каково" -> finiteClause A.n3 v ++ [mite $ Copula v0, semT (v "wh") "wh", semT v0 "degree", semV v0 "arg2" (v "wh"), mite $ ShortAdj (v "wh")]
  -- todo wh-questions with когда
  "когда" -> [mite $ ConditionComp v0 "when" False]
  "мной" -> pronoun Instr A.sg "ME" v
  "могут" -> finVerb "CAN" "PAST" A.pl3 v ++ [mite $ Control (v "theme"), semV v0 "theme" (v "theme")]
  "мое" -> [semT v0 "ME", mite $ Possessive Nom A.n v0]
  "удивительный" -> [mite $ Adj v0 Nom A.m "property" "AMAZING"]
  "обнаружили" -> finVerb "DISCOVER" "PAST" A.pl v ++ compHead "theme" v
  "он" -> pronoun Nom A.m3 "HE" v
  "они" -> pronoun Nom A.pl3 "THEY" v
  "отправился" -> finVerb "GO_OFF" "PAST" A.m v ++ arg KDat "goal" v
  "по" -> preposition PoDat Dat v
  "поводу" -> nounSg Dat Masc "MATTER" v
  "порядок" -> nounSg Acc Masc "ORDER" v ++ optional (arg Gen "arg1" v)
  "раньше" -> [mite $ ScalarAdverb "EARLIER" v0]
  "случай" -> nounSg Nom Masc "THING" v
  "случился" -> finVerb "HAPPEN" "PAST" A.m v ++ arg SInstr "experiencer" v
  "спросил" -> finVerb "ASK" "PAST" A.m v ++ arg Acc "arg2" v ++ compHead "topic" v
  "со" -> preposition SInstr Instr v
  "соседям" -> nounPl Dat "NEIGHBORS" v
  "счета" -> nounSg Gen Masc "COUNTING" v
  "удивление" -> nounSg Nom Neu "AMAZE" v
  "что" -> whWord v ++ xor [[mite $ Argument Nom v0], [mite $ Argument Acc v0]] ++ [mite $ AdjHead v0 Nom A.n3]
  "этому" -> [mite $ Adj v0 Dat A.sg "determiner" "THIS"]
  "я" -> pronoun Nom (A.Agr Nothing A.Sg $ Just 1) "ME" v
  "-" -> [mite $ QuestionVariants Nothing (Just "-")]
  _ -> [mite $ Word v0 word]
