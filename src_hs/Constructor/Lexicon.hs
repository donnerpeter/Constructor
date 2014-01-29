module Constructor.Lexicon where
import Constructor.Constructions
import qualified Constructor.Agreement as A
import Constructor.Agreement (Gender(..))
import Data.Char (ord, chr)
import Data.Maybe
import Data.List

nounSg caze gender typ v = pronoun caze (A.Agr (Just gender) A.Sg (Just 3)) typ v
nounPl caze typ v = pronoun caze (A.Agr Nothing A.Pl (Just 3)) typ v 
pronoun caze agr typ v = [mite $ Argument caze (v ""), semS (v "") "type" typ, mite $ AdjHead (v "") caze agr]
preposition prep nounArg v = [mite $ Argument (PP prep nounArg) (v ""), mite $ PrepHead nounArg (v "")]
finVerb typ time agr v = [semT (v "") typ, semS (v "") "time" time] ++ finiteClause agr v
finiteClause agr v = optional [mite $ NomHead agr (v "arg1")] ++ [semV (v "") "arg1" (v "arg1")] ++
                     (rusAgr (Just . A.number) "rusNumber") ++ (rusAgr A.gender "rusGender") ++ (rusAgr A.person "rusPerson") ++ 
                     clause v where
  rusAgr :: (Show a) => (A.Agr -> Maybe a) -> String -> [Mite]
  rusAgr f attr = maybeToList $ f agr >>= \x -> Just $ semS (v "arg1") attr (show x)
clause v = [mite $ Verb (v "")] ++
                (xor [[mite $ TopLevelClause (v "cp")], [mite $ SubordinateClause (v "cp")]]) ++
                (xor [[mite $ Fact (v "cp"), semT (v "cp") "fact"], [mite $ Question (v "cp") (v ""), semT (v "cp") "question"]]) ++
                [semV (v "cp") "content" (v "")]
infinitive typ v = [semT (v "") typ, mite $ Infinitive $ v "", mite $ Verb (v "")]
arg argType relation v = [mite $ ArgHead argType (v relation), semV (v "") relation (v relation)]
whWord v = [mite $ Wh (v "") (v "cp"), mite $ QuestionVariants (Just $ v "") Nothing,  semT (v "") "wh"]
optional mites = xor [mites, [mite $ EmptyCxt $ cxt $ head mites]]
compHead attr v = [mite $ CompHead (v "comp"), semV (v "") attr (v "comp")] 
adj caze agr attr value v = [mite $ Adj (v "") caze agr attr value]

wordMites :: String -> Int -> [Mite]  
wordMites word index =
  let v = \i -> Variable index i
      v0 = v ""
  in
  case word of
  s | length (reads s :: [(Int, String)]) == 1 -> xor [nounSg Nom Masc word v, nounSg Acc Masc word v] ++ [semS v0 "number" "true"]
  "было" -> [mite $ CopulaTense v0, semS v0 "time" "PAST"]
  "в" -> preposition "v" Acc v
  "вдруг" -> [mite $ Adverb "manner" "SUDDENLY"]
  "все" -> adj Nom A.pl "quantifier" "ALL" v
  "вспомнить" -> infinitive "RECALL" v ++ arg Acc "arg2" v
  "дальше" -> [mite $ Argument ScalarAdverb v0, semT v0 "NEXT"]
  "думают" -> finVerb "THINK" "PRESENT" A.pl3 v ++ arg Acc "arg2" v ++ arg (PP "po" Dat) "topic" v
  "забыл" -> finVerb "FORGET" "PAST" A.m v ++ compHead "arg2" v
  "забыли" -> finVerb "FORGET" "PAST" A.pl v ++ xor [compHead "arg2" v, 
      [mite $ ArgHead ScalarAdverb (v "scalar"), semV v0 "arg2" (v "arg2"), semT (v "arg2") "question", semV (v "arg2") "content" (v "comes"), semT (v "comes") "COME_SCALARLY", semV (v "comes") "arg1" (v "wh"), semT (v "wh") "wh", semV (v "comes") "order" (v "scalar")]
    ]
  "и" -> [mite $ Conjunction v0 "or", semS v0 "conj" "and", semT v0 "seq"]
  "идет" -> finVerb "COME_SCALARLY" "PRESENT" A.sg3 v ++ arg ScalarAdverb "order" v
  "или" -> [mite $ Conjunction v0 "or", semS v0 "conj" "or", semT v0 "seq"]
  "их" -> xor [pronoun Acc A.pl "THEY" v, [semT v0 "THEY", mite $ Possessive Nom A.sg v0], [semT v0 "THEY", mite $ Possessive Nom A.pl v0]]
  "к" -> preposition "k" Dat v
  -- todo wh-questions with каково
  "каково" -> finiteClause A.n3 v ++ [mite $ Copula v0, semT (v "wh") "wh", semT v0 "degree", semV v0 "arg2" (v "wh"), mite $ ShortAdj (v "wh")]
  -- todo wh-questions with когда
  "кассиршу" -> nounSg Acc Fem "CASHIER" v
  "когда" -> [mite $ ConditionComp v0 "when" False]
  "коммерческий" -> adj Acc A.m "kind" "COMMERCIAL" v
  -- todo который + agr
  "магазин" -> nounSg Acc Masc "SHOP" v
  "мной" -> pronoun Instr A.sg "ME" v
  "могут" -> finVerb "CAN" "PAST" A.pl3 v ++ [mite $ Control (v "theme"), semV v0 "theme" (v "theme")]
  "мое" -> [semT v0 "ME", mite $ Possessive Nom A.n v0]
  "мы" -> pronoun Nom A.pl1 "WE" v
  -- todo copula for prepositions besides 'na'
  "на" -> preposition "na" Prep v ++ optional (finiteClause A.sg (\s -> v $ 'x':s) ++ [mite $ Copula (v "x"), semT (v "x") "copula", semV (v "x") "location" v0]) 
  "нашем" -> [semT v0 "WE", mite $ Possessive Prep A.n v0]
  "недоумении" -> nounSg Prep Neu "PREDICAMENT" v
  "обнаружили" -> finVerb "DISCOVER" "PAST" A.pl v ++ compHead "theme" v
  "о" -> preposition "o" Prep v
  "он" -> pronoun Nom A.m3 "HE" v
  "они" -> pronoun Nom A.pl3 "THEY" v
  "отправился" -> finVerb "GO_OFF" "PAST" A.m v ++ arg (PP "k" Dat) "goal" v
  "по" -> preposition "po" Dat v
  "поводу" -> nounSg Dat Masc "MATTER" v
  "помнят" -> finVerb "REMEMBER" "PRESENT" A.pl3 v ++ arg Acc "arg2" v
  "порядок" -> nounSg Acc Masc "ORDER" v ++ optional (arg Gen "arg1" v)
  "пошли" -> finVerb "GO" "PAST" A.pl v ++ arg (PP "v" Acc) "goal" v
  "раньше" -> [mite $ Argument ScalarAdverb v0, semT v0 "EARLIER"]
  "случай" -> nounSg Nom Masc "THING" v
  "случился" -> finVerb "HAPPEN" "PAST" A.m v ++ arg (PP "s" Instr) "experiencer" v
  "спросил" -> finVerb "ASK" "PAST" A.m v ++ arg Acc "arg2" v ++ compHead "topic" v
  "спросили" -> finVerb "ASK" "PAST" A.pl v ++ arg Acc "arg2" v ++ xor [compHead "topic" v, arg (PP "o" Prep) "topic" v]
  "со" -> preposition "s" Instr v
  "соседям" -> nounPl Dat "NEIGHBORS" v
  "счета" -> nounSg Gen Masc "COUNTING" v
  "удивительный" -> adj Nom A.m "property" "AMAZING" v
  "углу" -> nounSg Prep Masc "CORNER" v ++ optional (arg Gen "arg1" v)
  "удивление" -> nounSg Nom Neu "AMAZE" v
  "улицы" -> nounSg Gen Fem "STREET" v
  "что" -> xor [whWord v ++ xor [[mite $ Argument Nom v0], [mite $ Argument Acc v0]] ++ [mite $ AdjHead v0 Nom A.n3], [mite $ Complementizer v0]]
  "этому" -> [mite $ Adj v0 Dat A.sg "determiner" "THIS"]
  "я" -> pronoun Nom (A.Agr Nothing A.Sg $ Just 1) "ME" v
  "-" -> [mite $ QuestionVariants Nothing (Just "-")]
  "," -> xor [[mite $ SurroundingComma v0], [mite $ Conjunction v0 ",", semT v0 "seq"]]
  "\"" -> xor [[mite $ Quote v0 True], [mite $ Quote v0 False]]
  _ ->
    if "ой" `isSuffixOf` word then 
      let nomName = take (length word - 2) word ++ "ая" in
      xor [adj Gen A.f "name" nomName v, nounSg Gen Fem "STREET" v ++ [semS v0 "name" nomName]]
    else [mite $ Word v0 word]
