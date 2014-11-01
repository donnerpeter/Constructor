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
import qualified Constructor.SemanticProperties as P

nounSg caze gender typ v = pronoun caze (A.Agr (Just gender) (Just A.Sg) (Just 3)) typ v
nounPl caze typ v = pronoun caze (A.Agr Nothing (Just A.Pl) (Just 3)) typ v
pronoun caze agr typ v = synNoun caze agr v ++ [semT (v "") typ] ++ rusGender agr (v "")

synNoun caze agr v = [mite $ Argument caze (v ""), mite $ AdjHead (v "") caze agr, mite $ RelativeHead (v "")]

rusGender agr v = case A.gender agr of
  Just g -> [semS v P.RusGender (show g)]
  _ -> []

rusPerson agr v = case A.person agr of
  Just g -> [semS v P.RusPerson (show g)]
  _ -> []

rusNumber agr v = case A.number agr of
  Just g -> [semS v P.RusNumber (show g)]
  _ -> []

preposition prep nounArg v = [mite $ PrepHead prep nounArg (v "")]
semPreposition nounArg attr v = [mite $ SemPreposition nounArg (v "noun"), semV (v "") attr (v "noun")]
finVerb typ time agr v = [semT (v "") typ, semS (v "") P.Time time] ++ finiteClause agr True v
raisingVerb typ time agr v = [semT (v "") typ, semS (v "") P.Time time, mite $ RaisingVerb (v "") (v "arg1")] ++ finiteClause agr False v
finiteClause agr withSemSubject v =
                     [mite $ NomHead agr (v "arg1") Unsatisfied, mite $ ReflexiveTarget (v "arg1")] ++
                     (if withSemSubject then [semV (v "") P.Arg1 (v "arg1")] else []) ++
                     rusNumber agr (v "arg1") ++ rusGender agr (v "arg1") ++ rusPerson agr (v "arg1") ++
                     clause v

clause v = [mite $ Verb (v ""), semV (v "cp") P.Content (v ""), semT (v "cp") "situation", mite $ Clause (v "cp")]

infinitive typ v =
  [semT (v "") typ] ++
  xor [[mite $ ControlledInfinitive $ v ""],
       [mite $ ModalityInfinitive (v "x") (v "cp"), semT (v "x") "modality", semV (v "x") P.Theme (v ""), semV (v "cp") P.Content (v "x"), mite $ Verb (v "x"), mite $ TenseHead (v "x")] ++ optional (arg Dat P.Arg1 v),
       semArg Direction P.Goal_action (v "")]
arg argType relation v = [mite $ ArgHead argType (v $ decapitalize $ show relation), semV (v "") relation (v $ decapitalize $ show relation)]
compHead attr v = [mite $ CompHead (v "comp"), semV (v "") attr (v "comp")]

semArg argType relation childVar@(Variable index s) = let headVar = Variable index (s ++ "_head") in
  [mite $ SemArgument argType headVar childVar, semV headVar relation childVar]

whWord v = [mite $ Wh (v ""), semT (v "") "wh"]
caseWhWord kind agr v = whWord v ++ [mite $ QuestionVariants (v "") kind, mite $ Argument kind (v ""), mite $ AdjHead (v "") kind agr]
negatedWh v = [semT (v "") "wh", semS (v "") P.Negated "true", mite $ Negated (v ""), mite $ NegativePronoun (v "")]
animate v = [semS (v "") P.Animate "true"]

adj caze agr attr value v = [mite $ Adj (v "") caze agr, semV (v "") attr (v "adj"), semT (v "adj") value]
  ++ rusNumber agr (v "")
adjWh caze agr attr v = [mite $ Adj (v "") caze agr, semV (v "") attr (v "adj"), semT (v "adj") "wh", mite $ Wh (v "adj")]
  ++ rusNumber agr (v "")

perfectBackground typ v = [mite $ Verb (v ""), semT (v "") typ, mite $ VerbalModifier P.PerfectBackground True (v "")]
sAdverb attr value v = [mite $ Adverb (v "verb"), semS (v "verb") attr value]
adverb attr value v = [mite $ Adverb (v "verb"), semV (v "verb") attr (v ""), semT (v "") value]
genHead attr v = optional [mite $ GenHead (v "gen"), semV (v "") attr (v "gen")]
directObject v = arg Acc P.Arg2 v
conjunction v0 conj ready = [mite $ Conjunction $ SeqData v0 conj ready False False False, semT v0 "seq"] ++ (if conj == "," then [] else [semS v0 P.Conj conj])

modifyV v c = \s -> v $ c:s
makeV (Variable index oldS) suffix = \s -> Variable index (oldS ++ suffix ++ s)

whatComesNext v = [mite $ ArgHead ScalarAdverb (v "scalar"),
  semV (v "") P.Arg2 (v "arg2"),
  semT (v "arg2") "situation", semV (v "arg2") P.Questioned (v "wh"), semV (v "arg2") P.Content (v "comes"),
  semT (v "comes") "COME_SCALARLY", semV (v "comes") P.Order (v "scalar"),
  semV (v "comes") P.Arg1 (v "wh"), semT (v "wh") "wh"]
numQuantifier ownCase childCase childAgr v = synNoun ownCase childAgr v ++ [semV (v "") P.Quantifier (v "q"), mite $ Quantifier childCase childAgr (v "")]
number word v = xor (concat [nounAlternatives caze ++ [quantifierAlternative caze] | caze <- [Nom, Gen, Acc]]) where
  nounAlternatives caze = [nounSg caze gender word v  ++ [semS (v "") P.Number "true"] | gender <- [Masc, Neu]]
  quantifierAlternative caze = numQuantifier caze (quantifierChildCase caze word) (quantifierChildAgr word) v ++ [semT (v "q") word, semS (v "q") P.Number "true"]
wordNumber caze typ v = xor [nounSg caze Masc typ v, numQuantifier caze (quantifierChildCase caze typ) (quantifierChildAgr typ) v ++ [semT (v "q") typ]]
quantifierChildCase caze typ = if typ /= "1" && (caze == Nom || caze == Acc) then Gen else caze
quantifierChildAgr typ = if typ `elem` ["1","2","3","4"] then A.sg else A.pl

go_args v = optional [mite $ SemArgHead Direction (v "")]

wordMites :: String -> Int -> [Mite]
wordMites word index =
  let v = \i -> Variable index i
      v0 = v ""
  in
  case word of
  s | isNumberString s -> number s v
  "5-ти" -> nounSg Gen Masc "5" v
  "6-ти" -> nounSg Gen Masc "6" v
  "а" -> xor [conjunction v0 "but" False, [mite $ ConjEmphasis P.AndEmphasis v0]]
  "арбуз" -> nounSg Nom Masc "WATERMELON" v
  "бессмысленными" -> [mite $ Raiseable A.pl v0, semT v0 "MEANINGLESS"]
  "большим" -> xor[adj Instr A.m P.Size "BIG" v, adj Instr A.n P.Size "BIG" v]
  "большой" -> xor[adj Instr A.f P.Size "BIG" v, adj Nom A.m P.Size "BIG" v, adj Acc A.m P.Size "BIG" v]
  "будет" -> [semS v0 P.Time "FUTURE"] ++ xor [[mite $ Tense v0], [mite $ WhAsserter v0], [mite $ FutureTense A.sg3 v0]]
  "был" -> [mite $ Tense v0, semS v0 P.Time "PAST"]
  "была" -> [mite $ Tense v0, semS v0 P.Time "PAST"]
  "было" -> [semS v0 P.Time "PAST"] ++ xor [[mite $ Tense v0], [mite $ WhAsserter v0]]
  "в" -> xor [preposition "v" Acc v, preposition "v" Prep v]
  "васе" -> xor [nounSg Dat Masc "NAMED_PERSON" v, nounSg Prep Masc "NAMED_PERSON" v] ++ [semS v0 P.Name "Vasya"]
  "васи" -> nounSg Gen Masc "NAMED_PERSON" v ++ [semS v0 P.Name "Vasya"]
  "вася" -> nounSg Nom Masc "NAMED_PERSON" v ++ [semS v0 P.Name "Vasya"]
  "вдруг" -> adverb P.Manner "SUDDENLY" v
  "вдумываясь" -> perfectBackground "THINK" v ++ arg (PP "v" Acc) P.Theme v
  "велел" -> finVerb "TO_ORDER" "PAST" A.m v ++ optional (arg Dat P.Arg2 v) ++ [mite $ Control (v "theme"), semV v0 P.Theme (v "theme")]
  "видел" -> finVerb "SEE" "PAST" A.m v ++ directObject v
  "восемь" -> xor [wordNumber Nom "8" v, wordNumber Acc "8" v]
  "восьми" -> wordNumber Gen "8" v
  "всякого" -> adj Gen A.m P.Determiner "ANY" v
  "выбежали" -> finVerb "RUN_OUT" "PAST" A.pl v ++ arg (PP "iz" Gen) P.Source v
  "вчера" -> adverb P.RelTime "YESTERDAY" v
  "вынул" -> finVerb "TAKE_OUT" "PAST" A.m v ++ arg (PP "iz" Gen) P.Source v ++ directObject v
  "вынула" -> finVerb "TAKE_OUT" "PAST" A.f v ++ arg (PP "iz" Gen) P.Source v ++ directObject v
  "все" -> xor [adj Nom A.pl P.Specifier_all "ALL" v, pronoun Nom A.pl "EVERYBODY" v ++ [mite $ UniversalPronoun v0]]
  "всё" -> xor [pronoun Nom A.n3 "EVERYTHING" v, pronoun Acc A.n3 "EVERYTHING" v] ++ [mite $ UniversalPronoun v0]
  "всеми" -> adj Instr A.pl P.Specifier_all "ALL" v ++ [mite $ UniversalPronoun v0]
  "вспомнить" -> infinitive "RECALL" v ++ directObject v
  "где" -> whWord v ++ [mite $ VerbalModifier P.Location False v0]
  "глазами" -> nounPl Instr "EYES" v ++ genHead P.Arg1 v
  "глупый" -> adj Nom A.m P.Quality "STUPID" v
  "грустно" -> adverb P.Manner "SADLY" v
  "гулять" -> infinitive "WALK" v
  "дальше" -> [mite $ Argument ScalarAdverb v0, semT v0 "NEXT"]
  "два" -> wordNumber Acc "2" v
  "делал" -> finVerb "DO" "PAST" A.m v ++ directObject v
  "делали" -> finVerb "DO" "PAST" A.pl v ++ directObject v
  "делать" -> infinitive "DO" v ++ directObject v
  "деньги" -> nounPl Acc "MONEY" v
  "деревья" -> nounPl Acc "TREES" v
  "десятью" -> wordNumber Instr "10" v
  "до" -> preposition "do" Gen v
  "дождь" -> nounSg Nom A.Masc "RAIN" v
  "дойдя" -> perfectBackground "COME_TO" v ++ optional (arg (PP "v" Prep) P.Domain v) ++ optional (arg (PP "do" Gen) P.Goal_by v)
  "долго" -> adverb P.Duration "LONG" v
  "дом" -> xor [nounSg Nom A.Masc "HOUSE" v, nounSg Acc A.Masc "HOUSE" v] ++ genHead P.Owner v
  "дома" -> xor [nounSg Gen A.Masc "HOUSE" v, nounPl Nom "HOUSES" v, nounPl Acc "HOUSES" v] ++ genHead P.Owner v
  "домам" -> nounPl Dat "HOUSES" v ++ genHead P.Owner v
  "доме" -> nounSg Prep A.Masc "HOUSE" v ++ genHead P.Owner v
  "домой" -> semArg Direction P.Goal v0 ++ [semT v0 "HOME"]
  "других" -> nounPl Gen "OTHERS" v
  "думает" -> finVerb "THINK" "PRESENT" A.sg3 v ++ optional (directObject v) ++ optional (arg (PP "po" Dat) P.Topic v)
  "думают" -> finVerb "THINK" "PRESENT" A.pl3 v ++ optional (directObject v) ++ optional (arg (PP "po" Dat) P.Topic v)
  "его" -> xor [pronoun Acc A.m "HE" v, [semT v0 "HE", mite $ Possessive Nom A.empty v0], [semT v0 "HE", mite $ Possessive Dat A.empty v0]]
  "ее" -> xor [pronoun Acc A.f "SHE" v, [semT v0 "SHE", mite $ Possessive Nom A.empty v0], [semT v0 "SHE", mite $ Possessive Dat A.empty v0]]
  "её" -> xor [pronoun Acc A.f "SHE" v, [semT v0 "SHE", mite $ Possessive Nom A.empty v0], [semT v0 "SHE", mite $ Possessive Dat A.empty v0]]
  "ей" -> pronoun Dat A.f "SHE" v
  "ему" -> pronoun Dat A.sg "HE" v
  "если" -> [mite $ ConditionComp v0 "if" False]
  "есть" -> [semS v0 P.Time "PRESENT"] ++ xor [[mite $ Tense v0], [mite $ WhAsserter v0]]
  "забыл" -> finVerb "FORGET" "PAST" A.m v ++ optional (xor [compHead P.Arg2 v, directObject v, whatComesNext v])
  "забыла" -> finVerb "FORGET" "PAST" A.f v ++ optional (xor [compHead P.Arg2 v, directObject v, whatComesNext v])
  "забыли" -> finVerb "FORGET" "PAST" A.pl v ++ optional (xor [compHead P.Arg2 v, directObject v, whatComesNext v])
  "знает" -> finVerb "KNOW" "PRESENT" A.sg3 v ++ optional (directObject v)
  "знают" -> finVerb "KNOW" "PRESENT" A.pl3 v ++ optional (directObject v)
  "и" -> conjunction v0 "and" True
  "идет" -> xor [finVerb "GO" "PRESENT" A.sg3 v ++ go_args v,
                 finVerb "WEATHER_BE" "PRESENT" A.sg3 v,
                 finVerb "COME_SCALARLY" "PRESENT" A.sg3 v ++ arg ScalarAdverb P.Order v]
  "идём" -> xor [finVerb "GO" "PRESENT" A.pl1 v ++ go_args v,
                 finVerb "WEATHER_BE" "PRESENT" A.pl1 v,
                 finVerb "COME_SCALARLY" "PRESENT" A.sg3 v ++ arg ScalarAdverb P.Order v]
  "идёт" -> xor [finVerb "GO" "PRESENT" A.sg3 v ++ go_args v,
                 finVerb "WEATHER_BE" "PRESENT" A.sg3 v,
                 finVerb "COME_SCALARLY" "PRESENT" A.sg3 v ++ arg ScalarAdverb P.Order v]
  "идти" -> xor [infinitive "GO" v ++ go_args v,
                 infinitive "WEATHER_BE" v,
                 infinitive "COME_SCALARLY" v ++ arg ScalarAdverb P.Order v]
  "из" -> preposition "iz" Gen v
  "изо" -> preposition "iz" Gen v
  "или" -> conjunction v0 "or" True
  "им" -> pronoun Dat A.f "THEY" v
  "их" -> xor [pronoun Acc A.pl "THEY" v,
               [semT v0 "THEY", mite $ Possessive Nom A.empty v0],
               [semT v0 "THEY", mite $ Possessive Gen A.empty v0],
               [semT v0 "THEY", mite $ Possessive Dat A.empty v0],
               [semT v0 "THEY", mite $ Possessive Acc A.empty v0]]
  "к" -> preposition "k" Dat v
  "кажется" -> raisingVerb "SEEM" "PRESENT" A.sg3 v ++ optional (arg Dat P.Experiencer v)
  "как" -> [mite $ TwoWordCxt "так как" False [] v0]
  "каково" ->
    finiteClause A.n3 True v ++ [mite $ TenseHead v0, semT (v "wh") "wh", semT v0 "degree", semV v0 P.Arg2 (v "wh"), mite $ ShortAdj (v "wh")]
  "какого" -> adjWh Gen A.m P.Determiner v
  "какой" -> xor [adjWh Nom A.m P.Determiner v, adjWh Acc A.m P.Determiner v]
  "какой-то" -> adj Nom A.sg P.Determiner "SOME" v
  "каком" -> adjWh Prep A.m P.Determiner v
  "кассир" -> nounSg Nom Masc "CASHIER" v ++ genHead P.Place v
  "кассирша" -> nounSg Nom Fem "CASHIER" v ++ genHead P.Place v
  "кассирши" -> nounSg Gen Fem "CASHIER" v ++ genHead P.Place v
  "кассиршу" -> nounSg Acc Fem "CASHIER" v ++ genHead P.Place v
  "квартирам" -> nounPl Dat "APARTMENTS" v ++ genHead P.Owner v
  "книга" -> nounSg Nom Fem "BOOK" v ++ genHead P.Author v
  "книгу" -> nounSg Acc Fem "BOOK" v ++ genHead P.Author v
  "кого" -> xor [caseWhWord Acc A.sg v ++ animate v, caseWhWord Gen A.sg v ++ animate v]
  "когда" -> xor [[mite $ ConditionComp v0 "when" False], whWord v ++ [mite $ VerbalModifier P.VTime False v0] ]
  "коммерческий" -> adj Acc A.m P.Kind "COMMERCIAL" v
  "комнатам" -> nounPl Dat "ROOMS" v ++ genHead P.Owner v
  "ком" -> caseWhWord Prep A.sg v ++ animate v
  "кому" -> caseWhWord Dat A.sg v ++ animate v
  "которого" -> xor [caseWhWord Gen A.m v, caseWhWord Gen A.n v, adjWh Gen A.m P.Determiner v]
  "котором" -> xor [caseWhWord Prep A.m v, caseWhWord Prep A.n v, adjWh Prep A.m P.Determiner v]
  "которую" -> xor [caseWhWord Acc A.f v, adjWh Acc A.f P.Determiner v]
  "кто" -> caseWhWord Nom A.sg v ++ animate v
  "куда" -> whWord v ++ semArg Direction P.Goal v0
  "летний" -> adj Acc A.m P.VName "летний" v -- todo летний is not only a name
  "лишенными" -> [mite $ Raiseable A.pl v0, semT v0 "LACK"] ++ arg Gen P.Theme v
  "любит" -> finVerb "LOVE" "PRESENT" A.sg3 v ++ optional (directObject v)
  "любить" -> infinitive "LOVE" v ++ optional (directObject v)
  "магазин" -> xor [nounSg Nom Masc "SHOP" v, nounSg Acc Masc "SHOP" v] -- todo который + agr
  "магазина" -> nounSg Gen Masc "SHOP" v
  "маленький" -> adj Acc A.m P.Size "LITTLE" v
  "меня" -> xor [pronoun Acc A.sg "ME" v, pronoun Gen A.sg "ME" v]
  "мне" -> pronoun Dat A.sg "ME" v
  "мнению" -> nounSg Dat Neu "OPINION" v ++ genHead P.Arg1 v
  "мной" -> pronoun Instr A.sg "ME" v
  "может" -> finVerb "CAN" "PRESENT" A.sg3 v ++ [mite $ Control (v "theme"), semV v0 P.Theme (v "theme")]
  "могут" -> finVerb "CAN" "PRESENT" A.pl3 v ++ [mite $ Control (v "theme"), semV v0 P.Theme (v "theme")]
  "молоточек" -> nounSg Acc Masc "HAMMER" v
  "мое" -> [semT v0 "ME", mite $ Possessive Nom A.n v0]
  "моему" -> [semT v0 "ME", mite $ Possessive Dat A.n v0]
  "мы" -> pronoun Nom A.pl1 "WE" v
  "на" -> xor [preposition "na" Prep v, preposition "na" Acc v]
  "нам" -> pronoun Dat A.pl1 "WE" v
  "нас" -> pronoun Acc A.pl1 "WE" v
  "начали" -> finVerb "BEGIN" "PAST" A.pl v ++ [mite $ Control (v "theme"), semV v0 P.Theme (v "theme")]
  "нашего" -> [semT v0 "WE", mite $ Possessive Gen A.m v0]
  "нашем" -> [semT v0 "WE", mite $ Possessive Prep A.n v0]
  "недоумении" -> nounSg Prep Neu "PREDICAMENT" v ++ genHead P.Arg1 v
  "некого" -> negatedWh v ++ [mite $ Argument Acc v0, mite $ ExistentialWh v0 (v "z")] ++ animate v
  "некому" -> negatedWh v ++ [mite $ Argument Dat v0, mite $ ExistentialWh v0 (v "z")] ++ animate v
  "некуда" -> negatedWh v ++ semArg Direction P.Goal v0 ++ [mite $ ExistentialWh v0 (v "z")]
  "нечего" -> negatedWh v ++ [mite $ Argument Acc v0, mite $ ExistentialWh v0 (v "z")]
  "никто" -> negatedWh v ++ [mite $ Argument Nom v0, mite $ AdjHead v0 Nom A.sg3] ++ animate v
  "никуда" -> negatedWh v ++ semArg Direction P.Goal v0
  "ничего" -> negatedWh v ++ [mite $ Argument Gen v0]
  "но" ->  xor [conjunction v0 "but" False, [mite $ ConjEmphasis P.ButEmphasis v0]]
  "носом" -> nounSg Instr Masc "NOSE" v
  "нужно" -> [semT v0 "NEED", mite $ NomHead A.n (v "arg2") Unsatisfied, semV v0 P.Arg2 (v "arg2"), mite $ TenseHead v0] ++ optional (arg Dat P.Arg1 v) ++ clause v
  "о" -> preposition "o" Prep v
  "обе" -> numQuantifier Acc Gen A.f v ++ [semT (v "q") "BOTH"]
  "облегчением" -> nounSg Instr Neu "RELIEF" v
  "обнаружил" -> finVerb "DISCOVER" "PAST" A.m v ++ compHead P.Theme v
  "обнаружила" -> finVerb "DISCOVER" "PAST" A.f v ++ compHead P.Theme v
  "обнаружили" -> finVerb "DISCOVER" "PAST" A.pl v ++ compHead P.Theme v
  "один" -> wordNumber Acc "1" v
  "одних" -> nounPl Gen "SOME" v
  "он" -> pronoun Nom A.m3 "HE" v
  "она" -> pronoun Nom A.f3 "SHE" v
  "они" -> pronoun Nom A.pl3 "THEY" v
  "опять" -> sAdverb P.SAnchor "AGAIN" v
  "остановились" -> finVerb "STOP" "PAST" A.pl v
  "от" -> preposition "ot" Gen v
  "отвлекло" -> finVerb "DISTRACT" "PAST" A.n v ++ directObject v ++ arg (PP "ot" Gen) P.Theme v
  "отправился" -> finVerb "GO_OFF" "PAST" A.m v ++ arg (PP "k" Dat) P.Goal_to v
  "офисам" -> nounPl Dat "OFFICES" v ++ genHead P.Owner v
  "палец" -> nounSg Acc Masc "FINGER" v
  "пальца" -> nounSg Gen Masc "FINGER" v
  "пальцами" -> nounPl Instr "FINGER" v
  "пальцев" -> nounPl Gen "FINGER" v
  "печатал" -> finVerb "TYPE" "PAST" A.m v ++ optional (arg Instr P.Instrument v)
  "по" -> preposition "po" Dat v
  "по-моему" -> xor [[mite $ VerbalModifier P.AccordingTo True v0], [mite $ NounAdjunct P.AccordingTo True v0]] ++ [semT v0 "OPINION", semV v0 P.Arg1 (v "me"), semT (v "me") "ME"]
  "поблагодарили" -> finVerb "THANK" "PAST" A.pl v ++ directObject v
  "поводу" -> nounSg Dat Masc "MATTER" v
  "дал" -> finVerb "TO_PRESENT" "PAST" A.m v ++ directObject v ++ optional (arg Dat P.Receiver v)
  "подвигав" -> perfectBackground "MOVE" v ++ arg Instr P.Arg2 v
  "подвигала" -> finVerb "MOVE" "PAST" A.f v ++ arg Instr P.Arg2 v
  "показались" -> raisingVerb "SEEM" "PAST" A.pl v ++ optional (arg Dat P.Experiencer v)
  "поливать" -> infinitive "TO_WATER" v ++ directObject v
  "помнит" -> finVerb "REMEMBER" "PRESENT" A.sg3 v ++ directObject v
  "помнят" -> finVerb "REMEMBER" "PRESENT" A.pl3 v ++ directObject v
  "помочь" -> infinitive "HELP" v ++ optional (arg Dat P.Arg2 v)
  "порядок" -> nounSg Acc Masc "ORDER" v ++ genHead P.Arg1 v
  "после" -> xor [[mite $ Argument ScalarAdverb v0, semT v0 "AFTER"], adverb P.RelTime "AFTER" v] ++ optional (semPreposition Gen P.Anchor v)
  "потом" -> xor [[mite $ Argument ScalarAdverb v0, semT v0 "NEXT"], adverb P.RelTime "AFTER" v]
  "потому" -> [mite $ TwoWordCxt "потому что" True [ReasonComp v0 False] v0]
  "приуныли" -> finVerb "GET_SAD" "PAST" A.pl v
  "про" -> preposition "pro" Acc v
  "просто" -> adverb P.Manner "JUST" v
  "пошла" -> finVerb "GO" "PAST" A.f3 v ++ [mite $ SemArgHead Direction v0]
  "пошли" -> finVerb "GO" "PAST" A.pl v ++ [mite $ SemArgHead Direction v0]
  "работу" -> nounSg Acc Fem "WORK" v
  "работы" -> nounSg Gen Fem "WORK" v
  "радостью" -> nounSg Instr Fem "JOY" v
  "разошлись" -> finVerb "DISPERSE" "PAST" A.pl v ++ arg (PP "po" Dat) P.Goal v
  "раньше" -> xor [[mite $ Argument ScalarAdverb v0, semT v0 "EARLIER"], adverb P.RelTime "BEFORE" v] ++ optional (semPreposition Gen P.Anchor v)
  "ребенок" -> nounSg Nom Masc "CHILD" v
  "речь" -> nounSg Nom Fem "SPEECH" v ++ genHead P.Arg1 v
  "рта" -> nounSg Gen Masc "MOUTH" v
  "с" -> xor [preposition "s" Instr v, preposition "s" Gen v]
  "сад" -> nounSg Acc Masc "GARDEN" v
  "сада" -> nounSg Gen Masc "GARDEN" v
  "свалился" -> finVerb "FALL" "PAST" A.m v ++ arg (PP "s" Gen) P.Source v
  "своим" -> [semT v0 "SELF", mite $ Possessive Dat A.pl v0, mite $ ReflexiveReference v0]
  "своими" -> [semT v0 "SELF", mite $ Possessive Instr A.pl v0, mite $ ReflexiveReference v0]
  "себе" -> pronoun Dat A.empty "SELF" v ++ [mite $ ReflexiveReference v0]
  "семи" -> wordNumber Gen "7" v
  "семь" -> xor [wordNumber Nom "7" v, wordNumber Acc "7" v]
  "семью" -> xor [nounSg Acc Fem "FAMILY" v ++ genHead P.Arg1 v, wordNumber Instr "7" v]
  "сидит" -> finVerb "SIT" "PRESENT" A.sg3 v
  "сидят" -> finVerb "SIT" "PRESENT" A.pl3 v
  "сказал" -> finVerb "SAY" "PAST" A.m v ++ optional (arg Dat P.Addressee v) ++ xor [[mite $ DirectSpeechHead v0 Nothing], directObject v, compHead P.Message v]
  "сказала" -> finVerb "SAY" "PAST" A.f v ++ optional (arg Dat P.Addressee v) ++ xor [[mite $ DirectSpeechHead v0 Nothing], directObject v, compHead P.Message v]
  "сказали" -> finVerb "SAY" "PAST" A.pl v ++ optional (arg Dat P.Addressee v) ++ xor [[mite $ DirectSpeechHead v0 Nothing], directObject v, compHead P.Message v]
  "сказать" -> infinitive "SAY" v ++ optional (arg Dat P.Addressee v) ++ xor [[mite $ DirectSpeechHead v0 Nothing], directObject v, compHead P.Message v]
  "скамейки" -> nounSg Gen Fem "BENCH" v
  "скромному" -> adj Dat A.n P.Quality "HUMBLE" v
  "слегка" -> adverb P.Manner "SLIGHTLY" v
  "следовало" -> finVerb "COME_SCALARLY" "PAST" A.n3 v ++ xor [arg ScalarAdverb P.Order v, arg (PP "posle" Gen) P.Order v]
  "слова" -> xor [nounPl Nom "WORDS" v, nounPl Acc "WORDS" v] ++ genHead P.Author v
  "словам" -> nounPl Dat "WORDS" v ++ genHead P.Author v
  "сломал" -> finVerb "BREAK" "PAST" A.m v ++ directObject v ++ arg Dat P.Receiver v
  "сломала" -> finVerb "BREAK" "PAST" A.f v ++ directObject v ++ arg Dat P.Receiver v
  "случай" -> nounSg Nom Masc "CASE" v
  "случае" -> nounSg Prep Masc "CASE" v ++ [mite $ ConditionCompHead v0]
  "случился" -> finVerb "HAPPEN" "PAST" A.m v ++ arg (PP "s" Instr) P.Experiencer v
  "смысла" -> nounSg Gen Masc "MEANING" v
  "снег" -> nounSg Nom Masc "SNOW" v
  "со" -> xor [preposition "s" Instr v, preposition "s" Gen v]
  "сонет" -> xor [nounSg Nom Masc "SONNET" v, nounSg Acc Masc "SONNET" v]
  "соседа" -> nounSg Gen Masc "NEIGHBOR" v
  "соседей" -> xor [nounPl Acc "NEIGHBORS" v, nounPl Gen "NEIGHBORS" v]
  "соседям" -> nounPl Dat "NEIGHBORS" v
  "спора" -> nounSg Gen Masc "ARGUE" v ++ genHead P.Arg1 v
  "спорили" -> finVerb "ARGUE" "PAST" A.pl v
  "спорить" -> infinitive "ARGUE" v
  "спросил" -> finVerb "ASK" "PAST" A.m v ++ optional (directObject v) ++ optional (xor [compHead P.Topic v, arg (PP "o" Prep) P.Topic v, arg (PP "pro" Acc) P.Topic v])
  "спросили" -> finVerb "ASK" "PAST" A.pl v ++ optional (directObject v) ++ optional (xor [compHead P.Topic v, arg (PP "o" Prep) P.Topic v, arg (PP "pro" Acc) P.Topic v])
  "спросить" -> infinitive "ASK" v ++ optional (directObject v) ++ optional (xor [compHead P.Topic v, arg (PP "o" Prep) P.Topic v])
  "стали" -> finVerb "BEGIN" "PAST" A.pl v ++ [mite $ Control (v "theme"), semV v0 P.Theme (v "theme")]
  "счастию" -> nounSg Dat Neu "LUCK" v
  "счета" -> nounSg Gen Masc "COUNTING" v
  "счете" -> nounSg Prep Masc "COUNTING" v
  "считать" -> infinitive "COUNT" v ++ directObject v
  "та" -> adj Nom A.f P.Determiner "THAT" v
  "так" -> [mite $ TwoWordCxt "так как" True [ReasonComp v0 False] v0]
  "там" -> adverb P.Location "THERE" v
  "танцевать" -> infinitive "DANCE" v
  "том" -> adj Prep A.sg P.Determiner "THAT" v
  "три" -> wordNumber Acc "3" v
  "тут" -> sAdverb P.Emphasis "true" v
  "у" -> preposition "u" Gen v
  "увидел" -> finVerb "SEE" "PAST" A.m v ++ directObject v ++ optional (arg Instr P.Instrument v)
  "удивительный" -> adj Nom A.m P.Property "AMAZING" v
  "углу" -> nounSg Prep Masc "CORNER" v ++ genHead P.Arg1 v
  "удивление" -> nounSg Nom Neu "AMAZE" v ++ genHead P.Arg1 v
  "уже" -> sAdverb P.SAnchor "ALREADY" v
  "улиц" -> nounPl Gen "STREETS" v
  "улицы" -> nounSg Gen Fem "STREET" v
  "улыбнулась" -> finVerb "SMILE" "PAST" A.f v
  "умная" -> adj Nom A.f P.Quality "CLEVER" v
  "умной" -> [mite $ Raiseable A.f v0, semT v0 "CLEVER"]
  "умные" -> adj Nom A.pl P.Quality "CLEVER" v
  "умный" -> adj Nom A.m P.Quality "CLEVER" v
  "челюсти" -> xor[nounSg Gen Fem "JAW" v, nounPl Nom "JAWS" v, nounPl Acc "JAWS" v]
  "челюсть" -> nounSg Acc Fem "JAW" v
  "челюстью" -> nounSg Instr Fem "JAW" v
  "чём" -> caseWhWord Prep A.n3 v
  "что" -> xor [caseWhWord Nom A.n3 v, caseWhWord Acc A.n3 v, [mite $ Complementizer v0], [mite $ TwoWordCxt "потому что" False [] v0], [mite $ Relativizer v0, semT v0 "wh"]]
  "чьему" -> whWord v ++ animate v ++ xor [[mite $ Possessive Dat A.m v0], [mite $ Possessive Dat A.n v0]]
  "чьим" -> whWord v ++ animate v ++ [mite $ Possessive Dat A.pl v0]
  "шести" -> wordNumber Gen "6" v
  "шел" -> xor [finVerb "GO" "PAST" A.m v ++ go_args v,
                 finVerb "WEATHER_BE" "PAST" A.m v,
                 finVerb "COME_SCALARLY" "PAST" A.m v ++ arg ScalarAdverb P.Order v] ++ [semS v0 P.Imperfective "true"]
  "шёл" -> xor [finVerb "GO" "PAST" A.m v ++ go_args v,
                 finVerb "WEATHER_BE" "PAST" A.m v,
                 finVerb "COME_SCALARLY" "PAST" A.m v ++ arg ScalarAdverb P.Order v] ++ [semS v0 P.Imperfective "true"]
  "эта" -> adj Nom A.f P.Determiner "THIS" v
  "это" -> xor [pronoun Nom (A.Agr (Just A.Neu) (Just A.Sg) $ Just 3) "THIS" v, pronoun Acc (A.Agr (Just A.Neu) (Just A.Sg) $ Just 3) "THIS" v]
  "этому" -> adj Dat A.sg P.Determiner "THIS" v
  "я" -> pronoun Nom (A.Agr Nothing (Just A.Sg) $ Just 1) "ME" v
  ":" -> xor [[mite $ Colon "directSpeech" v0], [mite $ Colon "elaboration" v0]]
  "-" -> xor [[mite $ SurroundingDash v0],
              [mite $ DirectSpeechDash v0],
              [mite $ Ellipsis v0 Nothing Nothing, semS v0 P.Ellipsis "true"]
             ]
  "," -> xor [[mite $ SurroundingComma v0], conjunction v0 "," True]
  "\"" -> xor [[mite $ Quote v0 True], [mite $ Quote v0 False]]
  _ ->
    if "ой" `isSuffixOf` word then 
      let nomName = take (length word - 2) word ++ "ая" in
      [semS v0 P.Name nomName] ++
      xor [[mite $ Adj (v "") Gen A.f],
           nounSg Gen Fem "STREET" v]
    else [mite $ Word v0 word]
