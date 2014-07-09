module Constructor.Lexicon where
import Constructor.Constructions
import Constructor.Mite
import Constructor.Variable
import Constructor.Util
import qualified Constructor.Agreement as A
import Constructor.Agreement (Gender(..))
import Data.Char (ord, chr)
import Data.Maybe
import Data.List

nounSg caze gender typ v = pronoun caze (A.Agr (Just gender) (Just A.Sg) (Just 3)) typ v
nounPl caze typ v = pronoun caze (A.Agr Nothing (Just A.Pl) (Just 3)) typ v
pronoun caze agr typ v = [mite $ Argument caze (v ""), semS (v "") "type" typ, mite $ AdjHead (v "") caze agr, mite $ RelativeHead (v "")]
preposition prep nounArg v = [mite $ PrepHead prep nounArg (v "")] ++ xor [[mite $ Argument (PP prep nounArg) (v "")], [mite $ ActivePreposition (v "")]]
semPreposition nounArg attr v = [mite $ SemPreposition nounArg (v "noun"), semV (v "") attr (v "noun")]
finVerb typ time agr v = [semT (v "") typ, semS (v "") "time" time] ++ finiteClause agr True v
raisingVerb typ time agr v = [semT (v "") typ, semS (v "") "time" time, mite $ RaisingVerb (v "") (v "arg1")] ++ finiteClause agr False v
finiteClause agr withSemSubject v = optional [mite $ NomHead agr (v "arg1") Unsatisfied] ++
                     [mite $ ReflexiveTarget (v "arg1")] ++
                     (if withSemSubject then [semV (v "") "arg1" (v "arg1")] else []) ++
                     (rusAgr A.number "rusNumber") ++ (rusAgr A.gender "rusGender") ++ (rusAgr A.person "rusPerson") ++
                     clause v where
  rusAgr :: (Show a) => (A.Agr -> Maybe a) -> String -> [Mite]
  rusAgr f attr = maybeToList $ f agr >>= \x -> Just $ semS (v "arg1") attr (show x)
clause v = [mite $ Verb (v ""), semV (v "cp") "content" (v "")] ++
           (xor [[mite $ Clause Declarative (v "cp"), semT (v "cp") "fact"], [mite $ Clause Interrogative (v "cp")]])
infinitive typ v =
  [semT (v "x") typ] ++ optional (arg Dat "arg1" $ modifyV v 'x') ++
  xor [[mite $ ControlledInfinitive $ v "", mite $ Unify (v "") (v "x")],
       [mite $ ModalityInfinitive (v "") (v "cp"), semT (v "") "modality", semV (v "") "theme" (v "x"), semV (v "cp") "content" (v ""), mite $ Verb (v ""), mite $ TenseHead (v "")]]
arg argType relation v = [mite $ ArgHead argType (v relation), semV (v "") relation (v relation)]
whWord kind v = [mite $ Wh (v ""), mite $ QuestionVariants (v "") kind, semT (v "") "wh"]
compHead attr v = [mite $ CompHead (v "comp"), semV (v "") attr (v "comp")] 
adj caze agr attr value v = [mite $ Adj (v "") caze agr, semV (v "") attr (v "adj"), semT (v "adj") value]
perfectBackground typ v = [mite $ Verb (v ""), semT (v "") typ, mite $ VerbalModifier "perfectBackground" True (v "")]
sAdverb attr value v = [mite $ Adverb (v "verb"), semS (v "verb") attr value]
adverb attr value v = [mite $ Adverb (v "verb"), semV (v "verb") attr (v ""), semT (v "") value]
genHead attr v = optional [mite $ GenHead (v "gen"), semV (v "") attr (v "gen")]
directObject v = arg Acc "arg2" v
conjunction v0 conj ready = [mite $ Conjunction $ SeqData v0 conj ready Nothing False Nothing, semT v0 "seq"] ++ (if conj == "," then [] else [semS v0 "conj" conj])
modifyV v c = \s -> v $ c:s 
whatComesNext v = [mite $ ArgHead ScalarAdverb (v "scalar"),
  semV (v "") "arg2" (v "arg2"),
  semT (v "arg2") "question", semV (v "arg2") "questioned" (v "wh"), semV (v "arg2") "content" (v "comes"),
  semT (v "comes") "COME_SCALARLY", semV (v "comes") "order" (v "scalar"),
  semV (v "comes") "arg1" (v "wh"), semT (v "wh") "wh"]
numQuantifier ownCase childCase childAgr v = [mite $ Argument ownCase (v ""), semV (v "") "quantifier" (v "q"), mite $ Quantifier childCase childAgr (v "")]
number word v = xor (concat [nounAlternatives caze ++ [quantifierAlternative caze] | caze <- [Nom, Gen, Acc]]) where
  nounAlternatives caze = [nounSg caze gender word v  ++ [semS (v "") "number" "true"] | gender <- [Masc, Neu]]
  quantifierAlternative caze = numQuantifier caze (quantifierChildCase caze word) (quantifierChildAgr word) v ++ [semT (v "q") word, semS (v "q") "number" "true"]
wordNumber caze typ v = xor [nounSg caze Masc typ v, numQuantifier caze (quantifierChildCase caze typ) (quantifierChildAgr typ) v ++ [semT (v "q") typ]]
quantifierChildCase caze typ = if typ == "1" then caze else Gen
quantifierChildAgr typ = if typ `elem` ["1","2","3","4"] then A.sg else A.pl

go_args v = optional (xor [arg (PP "v" Acc) "goal_in" v, arg DirectionAdverb "goal" v]) ++ optional (arg (PP "k" Dat) "goal_to" v)

wordMites :: String -> Int -> [Mite]
wordMites word index =
  let v = \i -> Variable index i
      v0 = v ""
  in
  case word of
  s | isNumberString s -> number s v
  "5-ти" -> nounSg Gen Masc "5" v
  "6-ти" -> nounSg Gen Masc "6" v
  "а" -> xor [conjunction v0 "but" False, [mite $ ConjEmphasis "andEmphasis" v0]]
  "арбуз" -> nounSg Nom Masc "WATERMELON" v
  "бессмысленными" -> [mite $ Raiseable A.pl v0, semT v0 "MEANINGLESS"]
  "большим" -> xor[adj Instr A.m "size" "BIG" v, adj Instr A.n "size" "BIG" v]
  "большой" -> xor[adj Instr A.f "size" "BIG" v, adj Nom A.m "size" "BIG" v, adj Acc A.m "size" "BIG" v]
  "будет" -> [semS v0 "time" "FUTURE"] ++ xor [[mite $ Tense v0], [mite $ WhAsserter v0]]
  "был" -> [mite $ Tense v0, semS v0 "time" "PAST"]
  "было" -> [semS v0 "time" "PAST"] ++ xor [[mite $ Tense v0], [mite $ WhAsserter v0]]
  "в" -> xor [preposition "v" Acc v, preposition "v" Prep v]
  "васи" -> nounSg Gen Masc "NAMED_PERSON" v ++ [semS v0 "name" "Vasya"]
  "вдруг" -> adverb "manner" "SUDDENLY" v
  "вдумываясь" -> perfectBackground "THINK" v ++ arg (PP "v" Acc) "theme" v
  "видел" -> finVerb "SEE" "PAST" A.m v ++ directObject v
  "восемь" -> xor [wordNumber Nom "8" v, wordNumber Acc "8" v]
  "восьми" -> wordNumber Gen "8" v
  "всякого" -> adj Gen A.m "determiner" "ANY" v
  "выбежали" -> finVerb "RUN_OUT" "PAST" A.pl v ++ arg (PP "iz" Gen) "source" v
  "вынул" -> finVerb "TAKE_OUT" "PAST" A.m v ++ arg (PP "iz" Gen) "source" v ++ directObject v
  "вынула" -> finVerb "TAKE_OUT" "PAST" A.f v ++ arg (PP "iz" Gen) "source" v ++ directObject v
  "все" -> adj Nom A.pl "quantifier" "ALL" v
  "вспомнить" -> infinitive "RECALL" v ++ directObject v
  "грустно" -> adverb "manner" "SADLY" v
  "дальше" -> [mite $ Argument ScalarAdverb v0, semT v0 "NEXT"]
  "два" -> wordNumber Acc "2" v
  "делал" -> finVerb "DO" "PAST" A.m v ++ directObject v
  "делали" -> finVerb "DO" "PAST" A.pl v ++ directObject v
  "делать" -> infinitive "DO" v ++ directObject v
  "деньги" -> nounPl Acc "MONEY" v
  "деревья" -> nounPl Acc "TREES" v
  "до" -> preposition "do" Gen v
  "дойдя" -> perfectBackground "COME_TO" v ++ optional (arg (PP "v" Prep) "domain" v) ++ optional (arg (PP "do" Gen) "goal_by" v)
  "долго" -> adverb "duration" "LONG" v
  "домам" -> nounPl Dat "HOMES" v ++ genHead "owner" v
  "домой" -> [mite $ Argument DirectionAdverb v0, semT v0 "HOME"]
  "других" -> nounPl Gen "OTHERS" v
  "думает" -> finVerb "THINK" "PRESENT" A.sg3 v ++ optional (directObject v) ++ optional (arg (PP "po" Dat) "topic" v)
  "думают" -> finVerb "THINK" "PRESENT" A.pl3 v ++ optional (directObject v) ++ optional (arg (PP "po" Dat) "topic" v)
  "его" -> xor [pronoun Acc A.m "HE" v, [semT v0 "HE", mite $ Possessive Nom A.empty v0], [semT v0 "HE", mite $ Possessive Dat A.empty v0]]
  "ее" -> xor [pronoun Acc A.f "SHE" v, [semT v0 "SHE", mite $ Possessive Nom A.empty v0], [semT v0 "SHE", mite $ Possessive Dat A.empty v0]]
  "её" -> xor [pronoun Acc A.f "SHE" v, [semT v0 "SHE", mite $ Possessive Nom A.empty v0], [semT v0 "SHE", mite $ Possessive Dat A.empty v0]]
  "ему" -> pronoun Dat A.sg "HE" v
  "если" -> [mite $ ConditionComp v0 "if" False]
  "есть" -> [semS v0 "time" "PRESENT"] ++ xor [[mite $ Tense v0], [mite $ WhAsserter v0]]
  "забыл" -> finVerb "FORGET" "PAST" A.m v ++ optional (xor [compHead "arg2" v, directObject v, whatComesNext v])
  "забыла" -> finVerb "FORGET" "PAST" A.f v ++ optional (xor [compHead "arg2" v, directObject v, whatComesNext v])
  "забыли" -> finVerb "FORGET" "PAST" A.pl v ++ optional (xor [compHead "arg2" v, directObject v, whatComesNext v])
  "и" -> conjunction v0 "and" True
  "идет" -> xor [finVerb "GO" "PRESENT" A.sg3 v ++ go_args v,
                 finVerb "COME_SCALARLY" "PRESENT" A.sg3 v ++ arg ScalarAdverb "order" v]
  "идём" -> xor [finVerb "GO" "PRESENT" A.pl1 v ++ go_args v,
                 finVerb "COME_SCALARLY" "PRESENT" A.sg3 v ++ arg ScalarAdverb "order" v]
  "идёт" -> xor [finVerb "GO" "PRESENT" A.sg3 v ++ go_args v,
                 finVerb "COME_SCALARLY" "PRESENT" A.sg3 v ++ arg ScalarAdverb "order" v]
  "идти" -> xor [infinitive "GO" v ++ go_args v,
                 infinitive "COME_SCALARLY" v ++ arg ScalarAdverb "order" v]
  "из" -> preposition "iz" Gen v
  "изо" -> preposition "iz" Gen v
  "или" -> conjunction v0 "or" True
  "их" -> xor [pronoun Acc A.pl "THEY" v, [semT v0 "THEY", mite $ Possessive Nom A.empty v0], [semT v0 "THEY", mite $ Possessive Gen A.empty v0], [semT v0 "THEY", mite $ Possessive Dat A.empty v0]]
  "к" -> preposition "k" Dat v
  "кажется" -> raisingVerb "SEEM" "PRESENT" A.sg3 v ++ optional (arg Dat "experiencer" v)
  "как" -> [mite $ TwoWordCxt "так как" False [] v0]
  "каково" -> 
    -- todo wh-questions with каково
    finiteClause A.n3 True v ++ [mite $ Copula v0, mite $ TenseHead v0, semT (v "wh") "wh", semT v0 "degree", semV v0 "arg2" (v "wh"), mite $ ShortAdj (v "wh")]
  "какой-то" -> adj Nom A.sg "determiner" "SOME" v
  "кассир" -> nounSg Nom Masc "CASHIER" v ++ genHead "place" v
  "кассирша" -> nounSg Nom Fem "CASHIER" v ++ genHead "place" v
  "кассирши" -> nounSg Gen Fem "CASHIER" v ++ genHead "place" v
  "кассиршу" -> nounSg Acc Fem "CASHIER" v ++ genHead "place" v
  "квартирам" -> nounPl Dat "APARTMENTS" v ++ genHead "owner" v
  "кого" -> xor [whWord Acc v ++ [mite $ Argument Acc v0, mite $ AdjHead v0 Acc A.sg, semS v0 "animate" "true"], whWord Gen v ++ [mite $ Argument Gen v0, mite $ AdjHead v0 Gen A.sg, semS v0 "animate" "true"]]
  "когда" -> [mite $ ConditionComp v0 "when" False] -- todo wh-questions with когда
  "коммерческий" -> adj Acc A.m "kind" "COMMERCIAL" v
  "комнатам" -> nounPl Dat "ROOMS" v ++ genHead "owner" v
  "кому" -> whWord Dat v ++ [mite $ Argument Dat v0, mite $ AdjHead v0 Dat A.sg, semS v0 "animate" "true"]
  "кто" -> whWord Nom v ++ [mite $ Argument Nom v0, mite $ AdjHead v0 Nom A.m3, semS v0 "animate" "true"]
  "куда" -> whWord DirectionAdverb v ++ [mite $ Argument DirectionAdverb v0]
  "летний" -> adj Acc A.m "name" "летний" v -- todo летний is not only a name
  "лишенными" -> [mite $ Raiseable A.pl v0, semT v0 "LACK"] ++ arg Gen "theme" v
  "любит" -> finVerb "LOVE" "PRESENT" A.sg3 v ++ optional (directObject v)
  "любить" -> infinitive "LOVE" v ++ optional (directObject v)
  "магазин" -> xor [nounSg Nom Masc "SHOP" v, nounSg Acc Masc "SHOP" v] -- todo который + agr
  "магазина" -> nounSg Gen Masc "SHOP" v
  "маленький" -> adj Acc A.m "size" "LITTLE" v
  "меня" -> xor [pronoun Acc A.sg "ME" v, pronoun Gen A.sg "ME" v]
  "мне" -> pronoun Dat A.sg "ME" v
  "мнению" -> nounSg Dat Neu "OPINION" v ++ genHead "arg1" v ++
    optional (xor [[mite $ PrepositionActivator "po" Dat v0 $ VerbalModifier "accordingTo" True v0],
                   [mite $ PrepositionActivator "po" Dat v0 $ NounAdjunct "accordingTo" True v0]])
  "мной" -> pronoun Instr A.sg "ME" v
  "может" -> finVerb "CAN" "PRESENT" A.sg3 v ++ [mite $ Control (v "theme"), semV v0 "theme" (v "theme")]
  "могут" -> finVerb "CAN" "PRESENT" A.pl3 v ++ [mite $ Control (v "theme"), semV v0 "theme" (v "theme")]
  "молоточек" -> nounSg Acc Masc "HAMMER" v
  "мое" -> [semT v0 "ME", mite $ Possessive Nom A.n v0]
  "моему" -> [semT v0 "ME", mite $ Possessive Dat A.n v0]
  "мы" -> pronoun Nom A.pl1 "WE" v
  "на" ->
    -- todo copula for prepositions besides 'na' 
    xor [preposition "na" Prep v, [mite $ PrepHead "na" Prep v0, mite $ Copula (v "x"), mite $ TenseHead (v "x"), semT (v "x") "copula", semV (v "x") "location" v0] ++ finiteClause A.empty True (modifyV v 'x')]
  "нам" -> pronoun Dat A.pl1 "WE" v
  "нас" -> pronoun Acc A.pl1 "WE" v
  "начали" -> finVerb "BEGIN" "PAST" A.pl v ++ [mite $ Control (v "theme"), semV v0 "theme" (v "theme")]
  "нашего" -> [semT v0 "WE", mite $ Possessive Gen A.m v0]
  "нашем" -> [semT v0 "WE", mite $ Possessive Prep A.n v0]
  "недоумении" -> nounSg Prep Neu "PREDICAMENT" v ++ genHead "arg1" v
  "некого" -> [mite $ Argument Acc v0, semT v0 "wh", mite $ ExistentialWh v0 (v "z"), semS v0 "animate" "true", semS v0 "negated" "true", mite $ Negated v0]
  "некому" -> [mite $ Argument Dat v0, semT v0 "wh", mite $ ExistentialWh v0 (v "z"), semS v0 "animate" "true", semS v0 "negated" "true", mite $ Negated v0]
  "некуда" -> [mite $ Argument DirectionAdverb v0, semT v0 "wh", mite $ ExistentialWh v0 (v "z"), semS v0 "negated" "true", mite $ Negated v0]
  "нечего" -> [mite $ Argument Acc v0, semT v0 "wh", mite $ ExistentialWh v0 (v "z"), semS v0 "negated" "true", mite $ Negated v0]
  "никто" -> [mite $ Argument Nom v0, mite $ AdjHead v0 Nom A.sg3, semT v0 "wh", semS v0 "negated" "true", semS v0 "animate" "true", mite $ Negated v0]
  "никуда" -> [mite $ Argument DirectionAdverb v0, semT v0 "wh", semS v0 "negated" "true", mite $ Negated v0]
  "ничего" -> [mite $ Argument Gen v0, semT v0 "wh", semS v0 "negated" "true", mite $ Negated v0]
  "но" ->  xor [conjunction v0 "but" False, [mite $ ConjEmphasis "butEmphasis" v0]]
  "носом" -> nounSg Instr Masc "NOSE" v
  "нужно" -> [semT v0 "NEED", mite $ NomHead A.n (v "arg2") Unsatisfied, semV v0 "arg2" (v "arg2"), mite $ Copula v0, mite $ TenseHead v0] ++ optional (arg Dat "arg1" v) ++ clause v
  "о" -> preposition "o" Prep v
  "обе" -> numQuantifier Acc Gen A.f v ++ [semT (v "q") "BOTH"]
  "облегчением" -> nounSg Instr Neu "RELIEF" v ++ optional [mite $ PrepositionActivator "s" Instr v0 $ VerbalModifier "mood" False v0]
  "обнаружил" -> finVerb "DISCOVER" "PAST" A.m v ++ compHead "theme" v
  "обнаружила" -> finVerb "DISCOVER" "PAST" A.f v ++ compHead "theme" v
  "обнаружили" -> finVerb "DISCOVER" "PAST" A.pl v ++ compHead "theme" v
  "один" -> wordNumber Acc "1" v
  "одних" -> nounPl Gen "SOME" v
  "он" -> pronoun Nom A.m3 "HE" v
  "она" -> pronoun Nom A.f3 "SHE" v
  "они" -> pronoun Nom A.pl3 "THEY" v
  "опять" -> sAdverb "anchor" "AGAIN" v
  "остановились" -> finVerb "STOP" "PAST" A.pl v
  "от" -> preposition "ot" Gen v
  "отвлекло" -> finVerb "DISTRACT" "PAST" A.n v ++ directObject v ++ arg (PP "ot" Gen) "theme" v
  "отправился" -> finVerb "GO_OFF" "PAST" A.m v ++ arg (PP "k" Dat) "goal_to" v
  "офисам" -> nounPl Dat "OFFICES" v ++ genHead "owner" v
  "палец" -> nounSg Acc Masc "FINGER" v
  "пальца" -> nounSg Gen Masc "FINGER" v
  "пальцев" -> nounPl Gen "FINGER" v
  "по" -> preposition "po" Dat v
  "по-моему" -> xor [[mite $ VerbalModifier "accordingTo" True v0], [mite $ NounAdjunct "accordingTo" True v0]] ++ [semT v0 "OPINION", semV v0 "arg1" (v "me"), semT (v "me") "ME"]
  "поблагодарили" -> finVerb "THANK" "PAST" A.pl v ++ directObject v
  "поводу" -> nounSg Dat Masc "MATTER" v
  "подвигав" -> perfectBackground "MOVE" v ++ arg Instr "arg2" v
  "подвигала" -> finVerb "MOVE" "PAST" A.f v ++ arg Instr "arg2" v
  "показались" -> raisingVerb "SEEM" "PAST" A.pl v ++ optional (arg Dat "experiencer" v)
  "поливать" -> infinitive "TO_WATER" v ++ directObject v
  "помнит" -> finVerb "REMEMBER" "PRESENT" A.sg3 v ++ directObject v
  "помнят" -> finVerb "REMEMBER" "PRESENT" A.pl3 v ++ directObject v
  "порядок" -> nounSg Acc Masc "ORDER" v ++ genHead "arg1" v
  "после" -> xor [[mite $ Argument ScalarAdverb v0, semT v0 "AFTER"], adverb "relTime" "AFTER" v] ++ optional (semPreposition Gen "anchor" v)
  "потом" -> xor [[mite $ Argument ScalarAdverb v0, semT v0 "NEXT"], adverb "relTime" "AFTER" v]
  "потому" -> [mite $ TwoWordCxt "потому что" True [ReasonComp v0 False] v0]
  "приуныли" -> finVerb "GET_SAD" "PAST" A.pl v
  "просто" -> adverb "manner" "JUST" v
  "пошли" -> finVerb "GO" "PAST" A.pl v ++ xor[arg (PP "v" Acc) "goal_in" v, arg DirectionAdverb "goal" v]
  "работы" -> nounSg Gen Fem "WORK" v
  "радостью" -> nounSg Instr Fem "JOY" v ++ optional [mite $ PrepositionActivator "s" Instr v0 $ VerbalModifier "mood" False v0]
  "разошлись" -> finVerb "DISPERSE" "PAST" A.pl v ++ arg (PP "po" Dat) "goal" v
  "раньше" -> xor [[mite $ Argument ScalarAdverb v0, semT v0 "EARLIER"], adverb "relTime" "BEFORE" v] ++ optional (semPreposition Gen "anchor" v)
  "ребенок" -> nounSg Nom Masc "CHILD" v
  "речь" -> nounSg Nom Fem "SPEECH" v ++ genHead "arg1" v
  "рта" -> nounSg Gen Masc "MOUTH" v
  "с" -> preposition "s" Instr v
  "сад" -> nounSg Acc Masc "GARDEN" v
  "сада" -> nounSg Gen Masc "GARDEN" v
  "свалился" -> finVerb "FALL" "PAST" A.m v ++ arg (PP "s" Gen) "source" v
  "своим" -> [semT v0 "SELF", mite $ Possessive Dat A.pl v0, mite $ ReflexiveReference v0]
  "себе" -> pronoun Dat A.empty "SELF" v ++ [mite $ ReflexiveReference v0]
  "семи" -> wordNumber Gen "7" v
  "семь" -> xor [wordNumber Nom "7" v, wordNumber Acc "7" v]
  "сидит" -> finVerb "SIT" "PRESENT" A.sg3 v
  "сидят" -> finVerb "SIT" "PRESENT" A.pl3 v
  "сказал" -> finVerb "SAY" "PAST" A.m v ++ optional (arg Dat "addressee" v) ++ xor [[mite $ DirectSpeechHead v0 Nothing], directObject v, compHead "message" v]
  "сказала" -> finVerb "SAY" "PAST" A.f v ++ optional (arg Dat "addressee" v) ++ xor [[mite $ DirectSpeechHead v0 Nothing], directObject v, compHead "message" v]
  "сказали" -> finVerb "SAY" "PAST" A.pl v ++ optional (arg Dat "addressee" v) ++ xor [[mite $ DirectSpeechHead v0 Nothing], directObject v, compHead "message" v]
  "сказать" -> infinitive "SAY" v ++ optional (arg Dat "addressee" v) ++ xor [[mite $ DirectSpeechHead v0 Nothing], directObject v, compHead "message" v]
  "скамейки" -> nounSg Gen Fem "BENCH" v
  "скромному" -> adj Dat A.n "quality" "HUMBLE" v
  "слегка" -> adverb "manner" "SLIGHTLY" v
  "следовало" -> finVerb "COME_SCALARLY" "PAST" A.n3 v ++ xor [arg ScalarAdverb "order" v, arg (PP "posle" Gen) "order" v]
  "слова" -> xor [nounPl Nom "WORDS" v, nounPl Acc "WORDS" v] ++ genHead "author" v
  "словам" -> nounPl Dat "WORDS" v ++ genHead "author" v ++
    optional (xor [[mite $ PrepositionActivator "po" Dat v0 $ VerbalModifier "accordingTo" True v0],
                   [mite $ PrepositionActivator "po" Dat v0 $ NounAdjunct "accordingTo" True v0]])
  "сломал" -> finVerb "BREAK" "PAST" A.m v ++ directObject v ++ arg Dat "receiver" v
  "сломала" -> finVerb "BREAK" "PAST" A.f v ++ directObject v ++ arg Dat "receiver" v
  "случай" -> nounSg Nom Masc "CASE" v
  "случае" -> nounSg Prep Masc "CASE" v ++ [mite $ ConditionCompHead v0] ++ optional [mite $ PrepositionActivator "v" Prep v0 $ VerbalModifier "condition" False v0]
  "случился" -> finVerb "HAPPEN" "PAST" A.m v ++ arg (PP "s" Instr) "experiencer" v
  "смысла" -> nounSg Gen Masc "MEANING" v
  "со" -> xor [preposition "s" Instr v, preposition "s" Gen v]
  "соседа" -> nounSg Gen Masc "NEIGHBOR" v
  "соседей" -> xor [nounPl Acc "NEIGHBORS" v, nounPl Gen "NEIGHBORS" v]
  "соседям" -> nounPl Dat "NEIGHBORS" v
  "спора" -> nounSg Gen Masc "ARGUE" v ++ genHead "arg1" v
  "спорили" -> finVerb "ARGUE" "PAST" A.pl v
  "спорить" -> infinitive "ARGUE" v
  "спросил" -> finVerb "ASK" "PAST" A.m v ++ optional (directObject v) ++ optional (xor [compHead "topic" v, arg (PP "o" Prep) "topic" v])
  "спросили" -> finVerb "ASK" "PAST" A.pl v ++ optional (directObject v) ++ optional (xor [compHead "topic" v, arg (PP "o" Prep) "topic" v])
  "спросить" -> infinitive "ASK" v ++ optional (directObject v) ++ optional (xor [compHead "topic" v, arg (PP "o" Prep) "topic" v])
  "стали" -> finVerb "BEGIN" "PAST" A.pl v ++ [mite $ Control (v "theme"), semV v0 "theme" (v "theme")]
  "счастию" -> nounSg Dat Neu "LUCK" v ++ optional [mite $ PrepositionActivator "po" Dat v0 $ VerbalModifier "optativeModality" True v0]
  "счета" -> nounSg Gen Masc "COUNTING" v
  "счете" -> nounSg Prep Masc "COUNTING" v
  "считать" -> infinitive "COUNT" v ++ directObject v
  "так" -> [mite $ TwoWordCxt "так как" True [ReasonComp v0 False] v0]
  "там" -> adverb "location" "THERE" v
  "танцевать" -> infinitive "DANCE" v
  "том" -> adj Prep A.sg "determiner" "THAT" v
  "три" -> wordNumber Acc "3" v
  "тут" -> sAdverb "emphasis" "true" v
  "у" ->
    -- todo copula for prepositions besides 'u'
    xor [preposition "u" Gen v, [mite $ PrepHead "u" Gen v0, mite $ Copula (v "x"), mite $ TenseHead (v "x"), semT (v "x") "copula", semV (v "x") "owner" v0] ++ finiteClause A.empty True (modifyV v 'x')]
  "удивительный" -> adj Nom A.m "property" "AMAZING" v
  "углу" -> nounSg Prep Masc "CORNER" v ++ genHead "arg1" v ++ optional [mite $ PrepositionActivator "na" Prep v0 $ NounAdjunct "location" False v0]
  "удивление" -> nounSg Nom Neu "AMAZE" v ++ genHead "arg1" v
  "уже" -> sAdverb "anchor" "ALREADY" v
  "улиц" -> nounPl Gen "STREETS" v
  "улицы" -> nounSg Gen Fem "STREET" v
  "улыбнулась" -> finVerb "SMILE" "PAST" A.f v
  "умной" -> [mite $ Raiseable A.f v0, semT v0 "CLEVER"]
  "челюсти" -> xor[nounSg Gen Fem "JAW" v, nounPl Nom "JAWS" v, nounPl Acc "JAWS" v]
  "челюсть" -> nounSg Acc Fem "JAW" v
  "челюстью" -> nounSg Instr Fem "JAW" v
  "чём" -> whWord Prep v ++ [mite $ Argument Prep v0, mite $ AdjHead v0 Prep A.n3]
  "что" -> xor [whWord Nom v ++ [mite $ Argument Nom v0, mite $ AdjHead v0 Nom A.n3],
                whWord Acc v ++ [mite $ Argument Acc v0, mite $ AdjHead v0 Acc A.n3],
                [mite $ Complementizer v0],
                [mite $ TwoWordCxt "потому что" False [] v0]]
  "чьему" -> whWord Gen v ++ [semS v0 "animate" "true"] ++ xor [[mite $ Possessive Dat A.m v0], [mite $ Possessive Dat A.n v0]]
  "шести" -> wordNumber Gen "6" v
  "это" -> xor [pronoun Nom (A.Agr (Just A.Neu) (Just A.Sg) $ Just 3) "THIS" v, pronoun Acc (A.Agr (Just A.Neu) (Just A.Sg) $ Just 3) "THIS" v]
  "этому" -> adj Dat A.sg "determiner" "THIS" v
  "я" -> pronoun Nom (A.Agr Nothing (Just A.Sg) $ Just 1) "ME" v
  ":" -> xor [[mite $ Colon "directSpeech" v0], [mite $ Colon "elaboration" v0]]
  "-" -> xor [[mite $ SurroundingDash False v0],
              [mite $ SurroundingDash True v0],
              [mite $ DirectSpeechDash v0],
              [mite $ Ellipsis v0 Nothing Nothing, semS v0 "ellipsis" "true"]
             ]
  "," -> xor [[mite $ SurroundingComma False v0], [mite $ SurroundingComma True v0], conjunction v0 "," True]
  "\"" -> xor [[mite $ Quote v0 True], [mite $ Quote v0 False]]
  _ ->
    if "ой" `isSuffixOf` word then 
      let nomName = take (length word - 2) word ++ "ая" in
      xor [[mite $ Adj (v "") Gen A.f, semS v0 "name" nomName], nounSg Gen Fem "STREET" v ++ [semS v0 "name" nomName]]
    else [mite $ Word v0 word]
