module Constructor.Lexicon where
import Constructor.Constructions
import Constructor.Variable
import qualified Constructor.Agreement as A
import Constructor.Agreement (Gender(..))
import Data.Char (ord, chr)
import Data.Maybe
import Data.List

nounSg caze gender typ v = pronoun caze (A.Agr (Just gender) A.Sg (Just 3)) typ v
nounPl caze typ v = pronoun caze (A.Agr Nothing A.Pl (Just 3)) typ v 
pronoun caze agr typ v = [mite $ Argument caze (v ""), semS (v "") "type" typ, mite $ AdjHead (v "") caze agr]
preposition prep nounArg v = [mite $ PrepHead prep nounArg (v "")] ++ xor [[mite $ Argument (PP prep nounArg) (v "")], [mite $ ActivePreposition (v "")]]
semPreposition prep nounArg typ attr v = [mite $ Argument (PP prep nounArg) (v ""), mite $ PrepHead prep nounArg (v "noun"), semT (v "") typ, semV (v "") attr (v "noun")]
finVerb typ time agr v = [semT (v "") typ, semS (v "") "time" time] ++ finiteClause agr True v
raisingVerb typ time agr v = [semT (v "") typ, semS (v "") "time" time, mite $ RaisingVerb (v "") (v "arg1")] ++ finiteClause agr False v
finiteClause agr withSemSubject v = optional [mite $ NomHead agr (v "arg1")] ++ 
                     (if withSemSubject then [semV (v "") "arg1" (v "arg1")] else []) ++
                     (rusAgr (Just . A.number) "rusNumber") ++ (rusAgr A.gender "rusGender") ++ (rusAgr A.person "rusPerson") ++ 
                     clause v where
  rusAgr :: (Show a) => (A.Agr -> Maybe a) -> String -> [Mite]
  rusAgr f attr = maybeToList $ f agr >>= \x -> Just $ semS (v "arg1") attr (show x)
clause v = [mite $ Verb (v "")] ++
                (xor [[mite $ TopLevelClause (v "cp")], [mite $ SubordinateClause (v "cp")]]) ++
                (xor [[mite $ Fact (v "cp"), semT (v "cp") "fact"], [mite $ Question (v "cp") (v ""), semT (v "cp") "question"]]) ++
                [semV (v "cp") "content" (v "")]
infinitive typ v =
  [semT (v "x") typ] ++ optional (arg Dat "arg1" $ modifyV v 'x') ++
  xor [[mite $ ControlledInfinitive $ v "", mite $ Unify (v "") (v "x")],
       [mite $ ModalityInfinitive (v ""), semT (v "") "modality", semV (v "") "theme" (v "x")] ++ clause v]
arg argType relation v = [mite $ ArgHead argType (v relation), semV (v "") relation (v relation)]
whWord v = [mite $ Wh (v "") (v "cp"), mite $ QuestionVariants (Just $ v "") Nothing,  semT (v "") "wh"]
optional mites = xor [mites, [mite $ EmptyCxt $ cxt $ head mites]]
compHead attr v = [mite $ CompHead (v "comp"), semV (v "") attr (v "comp")] 
adj caze agr attr value v = [mite $ Adj (v "") caze agr attr value]
perfectBackground typ v = [mite $ Verb (v ""), semT (v "") typ, mite $ VerbalModifier "perfectBackground" True (v "")]
adverb attr value = [mite $ Adverb attr value]
genHead attr v = optional [mite $ GenHead (v "gen"), semV (v "") attr (v "gen")]
directObject v = arg Acc "arg2" v
modifyV v c = \s -> v $ c:s 

isNumber s = length (reads s :: [(Int, String)]) == 1

wordMites :: String -> Int -> [Mite]  
wordMites word index =
  let v = \i -> Variable index i
      v0 = v ""
  in
  case word of
  s | isNumber s -> xor [nounSg Nom Masc word v, nounSg Acc Masc word v] ++ [semS v0 "number" "true"]
  "было" -> [mite $ CopulaTense v0, semS v0 "time" "PAST"]
  "в" -> xor [preposition "v" Acc v, preposition "v" Prep v]
  "вдруг" -> adverb "manner" "SUDDENLY"
  "вдумываясь" -> perfectBackground "THINK" v ++ arg (PP "v" Acc) "theme" v
  "восемь" -> nounSg Nom Masc "8" v
  "восьми" -> nounSg Gen Masc "8" v
  "всякого" -> adj Gen A.m "determiner" "ANY" v
  "выбежали" -> finVerb "RUN_OUT" "PAST" A.pl v ++ arg (PP "iz" Gen) "source" v
  "вынула" -> finVerb "TAKE_OUT" "PAST" A.f v ++ arg (PP "iz" Gen) "source" v ++ directObject v
  "все" -> adj Nom A.pl "quantifier" "ALL" v
  "вспомнить" -> infinitive "RECALL" v ++ directObject v
  "грустно" -> adverb "manner" "SADLY"
  "дальше" -> [mite $ Argument ScalarAdverb v0, semT v0 "NEXT"]
  "делать" -> infinitive "DO" v ++ directObject v
  "думают" -> finVerb "THINK" "PRESENT" A.pl3 v ++ directObject v ++ arg (PP "po" Dat) "topic" v
  "ее" -> xor [pronoun Acc A.pl "SHE" v, [semT v0 "SHE", mite $ Possessive Nom A.sg v0], [semT v0 "SHE", mite $ Possessive Nom A.pl v0]]
  "забыл" -> finVerb "FORGET" "PAST" A.m v ++ compHead "arg2" v
  "забыли" -> finVerb "FORGET" "PAST" A.pl v ++ xor [compHead "arg2" v, 
      [mite $ ArgHead ScalarAdverb (v "scalar"), semV v0 "arg2" (v "arg2"), semT (v "arg2") "question", semV (v "arg2") "content" (v "comes"), semT (v "comes") "COME_SCALARLY", semV (v "comes") "arg1" (v "wh"), semT (v "wh") "wh", semV (v "comes") "order" (v "scalar")]
    ]
  "и" -> [mite $ Conjunction v0 "and", semS v0 "conj" "and", semT v0 "seq"]
  "идет" -> finVerb "COME_SCALARLY" "PRESENT" A.sg3 v ++ xor [arg ScalarAdverb "order" v, arg (PP "posle" Gen) "order" v]
  "из" -> preposition "iz" Gen v
  "изо" -> preposition "iz" Gen v
  "или" -> [mite $ Conjunction v0 "or", semS v0 "conj" "or", semT v0 "seq"]
  "их" -> xor [pronoun Acc A.pl "THEY" v, [semT v0 "THEY", mite $ Possessive Nom A.sg v0], [semT v0 "THEY", mite $ Possessive Nom A.pl v0]]
  "к" -> preposition "k" Dat v
  "как" -> [mite $ TwoWordCxt "так как" False [] v0]
  "каково" -> 
    -- todo wh-questions with каково
    finiteClause A.n3 True v ++ [mite $ Copula v0, semT (v "wh") "wh", semT v0 "degree", semV v0 "arg2" (v "wh"), mite $ ShortAdj (v "wh")]
  "кассирша" -> nounSg Nom Fem "CASHIER" v
  "кассирши" -> nounSg Gen Fem "CASHIER" v
  "кассиршу" -> nounSg Acc Fem "CASHIER" v
  "когда" -> [mite $ ConditionComp v0 "when" False] -- todo wh-questions with когда
  "коммерческий" -> adj Acc A.m "kind" "COMMERCIAL" v
  "лишенными" -> [mite $ Raiseable A.pl v0, semT v0 "LACK"] ++ arg Gen "theme" v
  "магазин" -> nounSg Acc Masc "SHOP" v -- todo который + agr
  "магазина" -> nounSg Gen Masc "SHOP" v
  "маленький" -> adj Acc A.m "size" "LITTLE" v
  "мной" -> pronoun Instr A.sg "ME" v
  "могут" -> finVerb "CAN" "PAST" A.pl3 v ++ [mite $ Control (v "theme"), semV v0 "theme" (v "theme")]
  "молоточек" -> nounSg Acc Masc "HAMMER" v
  "мое" -> [semT v0 "ME", mite $ Possessive Nom A.n v0]
  "мы" -> pronoun Nom A.pl1 "WE" v
  "на" ->
    -- todo copula for prepositions besides 'na' 
    xor [preposition "na" Prep v, [mite $ PrepHead "na" Prep v0, mite $ PrepCopula v0, semT (v "x") "copula", semV (v "x") "location" v0] ++ finiteClause A.sg True (modifyV v 'x')] 
  "нам" -> pronoun Dat A.pl3 "WE" v
  "нашем" -> [semT v0 "WE", mite $ Possessive Prep A.n v0]
  "недоумении" -> nounSg Prep Neu "PREDICAMENT" v ++ genHead "arg1" v
  "но" ->  adverb "butEmphasis" "true"
  "носом" -> nounSg Instr Masc "NOSE" v
  "обнаружили" -> finVerb "DISCOVER" "PAST" A.pl v ++ compHead "theme" v
  "о" -> preposition "o" Prep v
  "он" -> pronoun Nom A.m3 "HE" v
  "они" -> pronoun Nom A.pl3 "THEY" v
  "опять" -> adverb "anchor" "AGAIN"
  "отправился" -> finVerb "GO_OFF" "PAST" A.m v ++ arg (PP "k" Dat) "goal" v
  "по" -> preposition "po" Dat v
  "по-моему" -> [mite $ VerbalModifier "accordingTo" True v0, semT v0 "OPINION", semV v0 "arg1" (v "me"), semT (v "me") "ME"]
  "поблагодарили" -> finVerb "THANK" "PAST" A.pl v ++ directObject v
  "поводу" -> nounSg Dat Masc "MATTER" v
  "подвигав" -> perfectBackground "MOVE" v ++ arg Instr "arg2" v
  "помнят" -> finVerb "REMEMBER" "PRESENT" A.pl3 v ++ directObject v
  "порядок" -> nounSg Acc Masc "ORDER" v ++ genHead "arg1" v
  "после" -> semPreposition "posle" Gen "AFTER" "anchor" v
  "показались" -> raisingVerb "SEEM" "PAST" A.pl v ++ arg Dat "experiencer" v
  "приуныли" -> finVerb "GET_SAD" "PAST" A.pl v
  "с" -> preposition "s" Instr v
  "пошли" -> finVerb "GO" "PAST" A.pl v ++ arg (PP "v" Acc) "goal" v
  "радостью" -> nounSg Instr Fem "JOY" v ++ optional [mite $ PrepositionActivator "s" Instr [VerbalModifier "mood" False v0]]
  "раньше" -> [mite $ Argument ScalarAdverb v0, semT v0 "EARLIER"]
  "рта" -> nounSg Gen Masc "MOUTH" v
  "семи" -> nounSg Gen Masc "7" v
  "семь" -> nounSg Nom Masc "7" v
  "сказала" -> finVerb "SAY" "PAST" A.f v ++ [mite $ DirectSpeechHead v0 Nothing] -- todo ++ directObject v
  "слегка" -> adverb "manner" "SLIGHTLY"
  "слова" -> xor [nounPl Nom "WORDS" v, nounPl Acc "WORDS" v] ++ genHead "author" v
  "случай" -> nounSg Nom Masc "CASE" v
  "случае" -> nounSg Prep Masc "CASE" v ++ [mite $ ConditionCompHead v0] ++ optional [mite $ PrepositionActivator "v" Prep [VerbalModifier "condition" False v0]]
  "случился" -> finVerb "HAPPEN" "PAST" A.m v ++ arg (PP "s" Instr) "experiencer" v
  "смысла" -> nounSg Gen Masc "MEANING" v
  "спросил" -> finVerb "ASK" "PAST" A.m v ++ directObject v ++ compHead "topic" v
  "спросили" -> finVerb "ASK" "PAST" A.pl v ++ directObject v ++ xor [compHead "topic" v, arg (PP "o" Prep) "topic" v]
  "со" -> preposition "s" Instr v
  "соседям" -> nounPl Dat "NEIGHBORS" v
  "счета" -> nounSg Gen Masc "COUNTING" v
  "так" -> [mite $ TwoWordCxt "так как" True [mite $ ReasonComp v0 False] v0]
  "том" -> [mite $ Adj v0 Prep A.sg "determiner" "THAT"]
  "тут" -> adverb "emphasis" "true"
  "удивительный" -> adj Nom A.m "property" "AMAZING" v
  "углу" -> nounSg Prep Masc "CORNER" v ++ genHead "arg1" v
  "удивление" -> nounSg Nom Neu "AMAZE" v ++ genHead "arg1" v
  "улицы" -> nounSg Gen Fem "STREET" v
  "улыбнулась" -> finVerb "SMILE" "PAST" A.f v
  "что" -> xor [whWord v ++ xor [[mite $ Argument Nom v0], [mite $ Argument Acc v0]] ++ [mite $ AdjHead v0 Nom A.n3], [mite $ Complementizer v0]]
  "этому" -> [mite $ Adj v0 Dat A.sg "determiner" "THIS"]
  "я" -> pronoun Nom (A.Agr Nothing A.Sg $ Just 1) "ME" v
  ":" -> xor [[mite $ Colon "directSpeech" v0], [mite $ Colon "elaboration" v0]]
  "-" -> xor [[mite $ QuestionVariants Nothing (Just "-")], [mite $ DirectSpeechDash v0]]
  "," -> xor [[mite $ SurroundingComma False v0], [mite $ SurroundingComma True v0], [mite $ Conjunction v0 ",", semT v0 "seq"]]
  "\"" -> xor [[mite $ Quote v0 True], [mite $ Quote v0 False]]
  _ ->
    if "ой" `isSuffixOf` word then 
      let nomName = take (length word - 2) word ++ "ая" in
      xor [adj Gen A.f "name" nomName v, nounSg Gen Fem "STREET" v ++ [semS v0 "name" nomName]]
    else [mite $ Word v0 word]
