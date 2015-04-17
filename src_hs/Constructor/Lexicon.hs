module Constructor.Lexicon where
import Constructor.Mite
import Constructor.Variable
import Constructor.Util
import Constructor.LexiconUtils
import qualified Constructor.Agreement as A
import Constructor.Agreement (Gender(..))
import Data.List
import qualified Constructor.SemanticProperties as P

wordMites :: String -> Int -> [Mite]
wordMites word index =
  let v = \i -> Variable index i
      v0 = v ""
  in
  case word of
  s | isNumberString s -> number s v
  "5-ти" -> nounSg Gen Masc "5" v
  "6-ти" -> nounSg Gen Masc "6" v
  "а" -> xor [conjunction v0 "a" False, [mite $ ConjEmphasis P.AndEmphasis v0]]
  "арбуз" -> nounSg Nom Masc "WATERMELON" v
  "бессмысленными" -> adj Instr A.pl P.Quality "MEANINGLESS" v
  "более" -> [mite $ Comparativizer v0]
  "больше" -> xor [comparativeAdj P.Size "BIG" v, [mite $ NegationModifier v0, semS v0 P.Not_anymore "true"]]
  "большим" -> xor[adj Instr A.m P.Size "BIG" v, adj Instr A.n P.Size "BIG" v]
  "большой" -> xor[adj Instr A.f P.Size "BIG" v, adj Nom A.m P.Size "BIG" v, adj Acc A.m P.Size "BIG" v]
  "брат" -> nounSg Nom Masc "BROTHER" v ++ genHead P.Arg1 v
  "бригадир" -> nounSg Nom Masc "BRIGADIER" v
  "будет" -> [semS v0 P.Time "FUTURE"] ++ xor [[mite $ Tense v0], [mite $ WhAsserter v0], [mite $ FutureTense A.sg3 v0]]
  "был" -> [mite $ Tense v0, semS v0 P.Time "PAST"]
  "была" -> [mite $ Tense v0, semS v0 P.Time "PAST"]
  "были" -> [mite $ Tense v0, semS v0 P.Time "PAST"]
  "было" -> [semS v0 P.Time "PAST"] ++ xor [[mite $ Tense v0], [mite $ WhAsserter v0]]
  "быстрее" -> comparativeAdj P.Quality "FAST" v
  "быстры" -> shortAdj A.pl P.Quality "FAST" v
  "в" -> xor [preposition "v" Acc v, preposition "v" Prep v]
  "варёную" -> adj Acc A.sg P.Kind "BOILED" v
  "васе" -> xor [nounSg Dat Masc "NAMED" v, nounSg Prep Masc "NAMED" v] ++ [semS v0 P.Name "Vasya"]
  "васи" -> nounSg Gen Masc "NAMED" v ++ [semS v0 P.Name "Vasya"]
  "василий" -> nounSg Nom Masc "NAMED" v ++ [semS v0 P.Name "Vassily"]
  "васю" -> nounSg Acc Masc "NAMED" v ++ [semS v0 P.Name "Vasya"]
  "вася" -> nounSg Nom Masc "NAMED" v ++ [semS v0 P.Name "Vasya"]
  "вашей" -> possessive Gen A.f "YOU" v
  "вдруг" -> adverb P.Manner "SUDDENLY" v
  "вдумываясь" -> perfectBackground "THINK" v ++ arg (PP "v" Acc) P.Theme v
  "велел" -> finVerb "TO_ORDER" "PAST" A.m v ++ optional (arg Dat P.Arg2 v) ++ [mite $ Control (v "theme"), semV v0 P.Theme (v "theme")]
  "видел" -> finVerb "SEE" "PAST" A.m v ++ directObject v
  "вниз" -> semArg Direction P.Goal v0 ++ [semT v0 "DOWN"]
  "восемь" -> xor [wordNumber Nom "8" v, wordNumber Acc "8" v]
  "восьми" -> wordNumber Gen "8" v
  "вот" -> sAdverb P.So_there "true" v
  "всякого" -> adj Gen A.m P.Determiner "ANY" v
  "вы" -> pronoun Nom A.empty "YOU" v
  "выбежали" -> finVerb "RUN_OUT" "PAST" A.pl v ++ arg (PP "iz" Gen) P.Source v
  "вываливающиеся" -> adj Nom A.pl P.Quality "FALL_OUT" v ++ optional (arg (PP "iz" Gen) P.Source v)
  "вывалилась" -> finVerb "FALL_OUT" "PAST" A.f v ++ optional (arg (PP "iz" Gen) P.Source v)
  "вывалился" -> finVerb "FALL_OUT" "PAST" A.m v ++ optional (arg (PP "iz" Gen) P.Source v)
  "высунулась" -> finVerb "LEAN_OUT" "PAST" A.f v ++ optional (arg (PP "iz" Gen) P.Source v)
  "вчера" -> adverb P.RelTime "YESTERDAY" v
  "вынул" -> finVerb "TAKE_OUT" "PAST" A.m v ++ arg (PP "iz" Gen) P.Source v ++ directObject v
  "вынула" -> finVerb "TAKE_OUT" "PAST" A.f v ++ arg (PP "iz" Gen) P.Source v ++ directObject v
  "все" -> xor [adj Nom A.pl P.Specifier_all "ALL" v, pronoun Nom A.pl "EVERYBODY" v ++ [mite $ UniversalPronoun v0]]
  "всего" -> [mite $ TwoWordCxt "всего лишь" True (map cxt $ modifierAdverb "ONLY" v) v0]
  "всё" -> xor [pronoun Nom A.n3 "EVERYTHING" v, pronoun Acc A.n3 "EVERYTHING" v] ++ [mite $ UniversalPronoun v0]
  "всеми" -> adj Instr A.pl P.Specifier_all "ALL" v ++ [mite $ UniversalPronoun v0]
  "вспомнить" -> infinitive "RECALL" v ++ directObject v
  "вязаную" -> adj Acc A.f P.Quality "WOVEN" v
  "где" -> whWord A.empty v ++ [mite $ VerbalModifier P.Location False v0]
  "глазами" -> nounPl Instr "EYES" v ++ genHead P.Arg1 v
  "глупый" -> adj Nom A.m P.Quality "STUPID" v
  "говорят" -> [mite $ VerbalModifier P.AccordingTo True v0, semT v0 "SAY"]
  "грустно" -> adverb P.Manner "SADLY" v
  "гулять" -> infinitive "WALK" v
  "дал" -> finVerb "GIVE" "PAST" A.m v ++ directObject v ++ optional (arg Dat P.Receiver v)
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
  "дочь" -> xor [nounSg Nom Fem "DAUGHTER" v, nounSg Acc Fem "DAUGHTER" v]
  "другая" -> adj Nom A.f P.Determiner "ANOTHER" v
  "других" -> nounPl Gen "OTHERS" v
  "думает" -> finVerb "THINK" "PRESENT" A.sg3 v ++ optional (directObject v) ++ optional (arg (PP "po" Dat) P.Topic v)
  "думают" -> finVerb "THINK" "PRESENT" A.pl3 v ++ optional (directObject v) ++ optional (arg (PP "po" Dat) P.Topic v)
  "его" -> xor $ [pronoun Acc A.m "HE" v, pronoun Gen A.m "HE" v] ++ map (\c -> possessive c A.empty "HE" v) cases
  "ее" -> xor $ [pronoun Acc A.f "SHE" v, pronoun Gen A.m "SHE" v] ++ map (\c -> possessive c A.empty "SHE" v) cases
  "её" -> xor $ [pronoun Acc A.f "SHE" v, pronoun Gen A.m "SHE" v] ++ map (\c -> possessive c A.empty "SHE" v) cases
  "ей" -> pronoun Dat A.f "SHE" v
  "ему" -> pronoun Dat A.sg "HE" v
  "если" -> [mite $ ConditionComp v0 "if" False]
  "есть" -> [semS v0 P.Time "PRESENT"] ++ xor [[mite $ Tense v0], [mite $ WhAsserter v0]]
  "ещё" -> [mite $ ComparativeEmphasis v0]
  "жареную" -> adj Acc A.sg P.Kind "ROASTED" v
  "забыл" -> finVerb "FORGET" "PAST" A.m v ++ optional (xor [compHead P.Arg2 v, directObject v, arg ScalarAdverb P.Order v])
  "забыла" -> finVerb "FORGET" "PAST" A.f v ++ optional (xor [compHead P.Arg2 v, directObject v, arg ScalarAdverb P.Order v])
  "забыли" -> finVerb "FORGET" "PAST" A.pl v ++ optional (xor [compHead P.Arg2 v, directObject v, arg ScalarAdverb P.Order v])
  "завтра" -> adverb P.RelTime "TOMORROW" v
  "закрыты" -> shortAdj A.pl P.State "CLOSED" v
  "зелёный" -> adj Nom A.m P.Color "GREEN" v
  "знает" -> finVerb "KNOW" "PRESENT" A.sg3 v ++ optional (directObject v)
  "знают" -> finVerb "KNOW" "PRESENT" A.pl3 v ++ optional (directObject v)
  "и" -> xor [conjunction v0 "i" True, [mite $ AndEmphasis v0]]
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
  "или" -> conjunction v0 "ili" True
  "им" -> pronoun Dat A.f "THEY" v
  "их" -> xor $ [pronoun Acc A.pl "THEY" v] ++ map (\c -> possessive c A.empty "THEY" v) cases
  "к" -> preposition "k" Dat v
  "кажется" -> raisingVerb "SEEM" "PRESENT" A.sg3 v ++ optional (arg Dat P.Experiencer v)
  "как" -> [mite $ TwoWordCxt "так как" False [] v0]
  "каков" -> [mite $ ShortAdj A.m P.Quality v0, mite $ ParticleEmphasizeable v0] ++ whWord A.m v
  "каково" -> [mite $ ShortAdj A.n P.Quality v0, mite $ ParticleEmphasizeable v0] ++ whWord A.n v
  "какого" -> adjWh Gen A.m P.Determiner v
  "какой" -> xor [adjWh Nom A.m P.Determiner v, adjWh Acc A.m P.Determiner v]
  "какой-то" -> adj Nom A.sg P.Determiner "SOME" v
  "каком" -> adjWh Prep A.m P.Determiner v
  "капусту" -> nounSg Acc Fem "CABBAGE" v
  "кассир" -> nounSg Nom Masc "CASHIER" v ++ genHead P.Place v
  "кассира" -> nounSg Gen Masc "CASHIER" v ++ genHead P.Place v
  "кассирша" -> nounSg Nom Fem "CASHIER" v ++ genHead P.Place v
  "кассирши" -> nounSg Gen Fem "CASHIER" v ++ genHead P.Place v
  "кассиршу" -> nounSg Acc Fem "CASHIER" v ++ genHead P.Place v
  "кассиром" -> nounSg Instr Masc "CASHIER" v ++ genHead P.Place v
  "квартирам" -> nounPl Dat "APARTMENTS" v ++ genHead P.Owner v
  "кем" -> caseWhWord Instr A.empty v ++ animate v
  "книга" -> nounSg Nom Fem "BOOK" v ++ genHead P.Author v
  "книгу" -> nounSg Acc Fem "BOOK" v ++ genHead P.Author v
  "кого" -> xor [caseWhWord Acc A.sg v ++ animate v, caseWhWord Gen A.sg v ++ animate v]
  "когда" -> xor [[mite $ ConditionComp v0 "when" False], whWord A.empty v ++ [mite $ VerbalModifier P.VTime False v0] ]
  "коммерческий" -> adj Acc A.m P.Kind "COMMERCIAL" v
  "комнатам" -> nounPl Dat "ROOMS" v ++ genHead P.Owner v
  "ком" -> caseWhWord Prep A.sg v ++ animate v
  "кому" -> caseWhWord Dat A.sg v ++ animate v
  "которого" -> xor [caseWhWord Gen A.m v, caseWhWord Gen A.n v, adjWh Gen A.m P.Determiner v, caseWhWord Acc A.m v, adjWh Acc A.m P.Determiner v]
  "котором" -> xor [caseWhWord Prep A.m v, caseWhWord Prep A.n v, adjWh Prep A.m P.Determiner v]
  "которую" -> xor [caseWhWord Acc A.f v, adjWh Acc A.f P.Determiner v]
  "который" -> xor [caseWhWord Nom A.m v, adjWh Nom A.m P.Determiner v]
  "красный" -> adj Nom A.m P.Color "RED" v
  "кстати" -> [mite $ VerbalModifier P.OptativeModality True v0, semT v0 "BY_THE_WAY"]
  "кто" -> caseWhWord Nom A.sg v ++ animate v
  "куда" -> whWord A.empty v ++ semArg Direction P.Goal v0
  "летний" -> adj Acc A.m P.VName "name" v ++ [semS v0 P.Name "летний"] -- todo летний is not only a name
  "лишенными" -> adj Instr A.pl P.Quality "LACK" v ++ arg Gen P.Theme v
  "лишь" -> xor [modifierAdverb "ONLY" v, [mite $ TwoWordCxt "всего лишь" False [] v0]]
  "лучше" -> comparativeAdj P.Quality "GOOD" v
  "любит" -> finVerb "LOVE" "PRESENT" A.sg3 v ++ optional (directObject v)
  "любить" -> infinitive "LOVE" v ++ optional (directObject v)
  "люблю" -> finVerb "LOVE" "PRESENT" A.sg1 v ++ optional (directObject v)
  "любопытства" -> nounSg Gen Neu "CURIOSITY" v
  "магазин" -> xor [nounSg Nom Masc "SHOP" v, nounSg Acc Masc "SHOP" v]
  "магазина" -> nounSg Gen Masc "SHOP" v
  "магазине" -> nounSg Prep Masc "SHOP" v
  "маленький" -> adj Acc A.m P.Size "LITTLE" v
  "мать" -> xor [nounSg Nom Fem "MOTHER" v, nounSg Acc Fem "MOTHER" v]
  "маша" -> nounSg Nom Fem "NAMED" v ++ [semS v0 P.Name "Masha"]
  "меня" -> xor [pronoun Acc A.sg "ME" v, pronoun Gen A.sg "ME" v]
  "мне" -> pronoun Dat A.sg "ME" v
  "мнение" -> nounSg Nom Neu "OPINION" v ++ genHead P.Arg1 v
  "мнению" -> nounSg Dat Neu "OPINION" v ++ genHead P.Arg1 v
  "мной" -> pronoun Instr A.sg "ME" v
  "может" -> finVerb "CAN" "PRESENT" A.sg3 v ++ [mite $ Control (v "theme"), semV v0 P.Theme (v "theme")]
  "могут" -> finVerb "CAN" "PRESENT" A.pl3 v ++ [mite $ Control (v "theme"), semV v0 P.Theme (v "theme")]
  "молоточек" -> nounSg Acc Masc "HAMMER" v
  "мое" -> possessive Nom A.n "ME" v
  "моему" -> possessive Dat A.n "ME" v
  "морковку" -> nounSg Acc Fem "CARROT" v
  "моя" -> possessive Nom A.f "ME" v
  "мы" -> pronoun Nom A.pl1 "WE" v
  "на" -> xor [preposition "na" Prep v, preposition "na" Acc v]
  "надоел" -> [semT v0 "PESTER", mite $ Verb v0] ++ clause v ++ optional (arg Dat P.Arg2 v)
     ++ xor [[mite $ Control (v "arg1"), semV v0 P.Arg1 (v "arg1"), mite $ NomHead A.m (v "arg1") Satisfied],
             [mite $ NomHead A.m (v "arg1") Unsatisfied, semV v0 P.Arg1 (v "arg1")]]
  "надоела" -> [semT v0 "PESTER", mite $ Verb v0] ++ clause v ++ optional (arg Dat P.Arg2 v)
     ++ xor [[mite $ Control (v "arg1"), semV v0 P.Arg1 (v "arg1"), mite $ NomHead A.f (v "arg1") Satisfied],
             [mite $ NomHead A.f (v "arg1") Unsatisfied, semV v0 P.Arg1 (v "arg1")]]
  "надоело" -> [semT v0 "PESTER", mite $ Verb v0] ++ clause v ++ optional (arg Dat P.Arg2 v)
     ++ xor [[mite $ Control (v "arg1"), semV v0 P.Arg1 (v "arg1"), mite $ NomHead A.n (v "arg1") Satisfied],
             [mite $ NomHead A.m (v "arg1") Unsatisfied, semV v0 P.Arg1 (v "arg1")]]
  "нам" -> pronoun Dat A.pl1 "WE" v
  "нас" -> xor [pronoun Acc A.pl1 "WE" v, pronoun Gen A.pl1 "WE" v]
  "начал" -> finVerb "BEGIN" "PAST" A.m v ++ [mite $ Control (v "theme"), semV v0 P.Theme (v "theme")]
  "начали" -> finVerb "BEGIN" "PAST" A.pl v ++ [mite $ Control (v "theme"), semV v0 P.Theme (v "theme")]
  "нашего" -> possessive Gen A.m "WE" v
  "нашем" -> possessive Prep A.n "WE" v
  "недоумении" -> nounSg Prep Neu "PREDICAMENT" v ++ genHead P.Arg1 v
  "некого" -> negatedWh v ++ [mite $ Argument Acc v0, mite $ ExistentialWh v0 (v "z")] ++ animate v
  "некому" -> negatedWh v ++ [mite $ Argument Dat v0, mite $ ExistentialWh v0 (v "z")] ++ animate v
  "некуда" -> negatedWh v ++ semArg Direction P.Goal v0 ++ [mite $ ExistentialWh v0 (v "z")]
  "необъятного" -> adj Gen A.m P.Size "UNEMBRACEABLE" v
  "нечего" -> negatedWh v ++ [mite $ Argument Acc v0, mite $ ExistentialWh v0 (v "z")]
  "неумны" -> shortAdj A.pl P.Quality "CLEVER" v ++ [semS (v "") P.Negated "true"]
  "никто" -> negatedWh v ++ [mite $ Argument Nom v0, mite $ AdjHead v0 Nom A.sg3] ++ animate v
  "никуда" -> negatedWh v ++ semArg Direction P.Goal v0
  "них" -> pronoun Acc A.pl "THEY" v
  "ничего" -> negatedWh v ++ [mite $ Argument Gen v0]
  "но" ->  xor [conjunction v0 "no" False ++ [semS v0 P.ConjStrong "true"], [mite $ ConjEmphasis P.ButEmphasis v0]]
  "носом" -> nounSg Instr Masc "NOSE" v
  "нужно" -> [semT v0 "NEED", mite $ NomHead A.n (v "arg2") Unsatisfied, semV v0 P.Arg2 (v "arg2"), mite $ TenseHead Optional v0] ++ optional (arg Dat P.Arg1 v) ++ clause v
  "о" -> preposition "o" Prep v
  "обе" -> numQuantifier Acc Gen A.f v ++ [semT v0 "BOTH"]
  "облегчением" -> nounSg Instr Neu "RELIEF" v
  "обнаружил" -> finVerb "DISCOVER" "PAST" A.m v ++ compHead P.Theme v
  "обнаружила" -> finVerb "DISCOVER" "PAST" A.f v ++ compHead P.Theme v
  "обнаружили" -> finVerb "DISCOVER" "PAST" A.pl v ++ compHead P.Theme v
  "обнимет" -> finVerb "EMBRACE" "FUTURE" A.sg3 v ++ directObject v
  "один" -> xor [wordNumber Acc "1" v, adj Nom A.m P.Determiner "ONE" v]
  "одна" -> adj Nom A.f P.Determiner "ONE" v
  "одних" -> nounPl Gen "SOME" v
  "одному" -> adj Dat A.m P.Determiner "ONE" v
  "одностороння" -> shortAdj A.f P.Quality "UNILATERAL" v
  "окна" -> xor [nounSg Gen Neu "WINDOW" v, nounPl Nom "WINDOWS" v] ++ genHead P.Owner v
  "он" -> pronoun Nom A.m3 "HE" v
  "она" -> pronoun Nom A.f3 "SHE" v
  "они" -> pronoun Nom A.pl3 "THEY" v
  "опять" -> sAdverb P.SAnchor "AGAIN" v
  "остановились" -> finVerb "STOP" "PAST" A.pl v
  "от" -> preposition "ot" Gen v
  "отвлекло" -> finVerb "DISTRACT" "PAST" A.n v ++ directObject v ++ arg (PP "ot" Gen) P.Theme v
  "отправился" -> finVerb "GO_OFF" "PAST" A.m v ++ arg (PP "k" Dat) P.Goal_to v
  "офисам" -> nounPl Dat "OFFICES" v ++ genHead P.Owner v
  "очень" -> modifierAdverb "VERY" v
  "палец" -> nounSg Acc Masc "FINGER" v
  "пальца" -> nounSg Gen Masc "FINGER" v
  "пальцами" -> nounPl Instr "FINGER" v
  "пальцев" -> nounPl Gen "FINGER" v
  "печатал" -> finVerb "TYPE" "PAST" A.m v ++ optional (arg Instr P.Instrument v)
  "по" -> preposition "po" Dat v
  "по-моему" -> xor [[mite $ VerbalModifier P.AccordingTo True v0], [mite $ NounAdjunct P.AccordingTo True v0]] ++ [semT v0 "OPINION", semV v0 P.Arg1 (v "me"), semT (v "me") "ME"]
  "поблагодарили" -> finVerb "THANK" "PAST" A.pl v ++ directObject v
  "поводу" -> nounSg Dat Masc "MATTER" v
  "подарили" -> finVerb "TO_PRESENT" "PAST" A.pl v ++ directObject v ++ optional (arg Dat P.Receiver v)
  "подвигав" -> perfectBackground "MOVE" v ++ arg Instr P.Arg2 v
  "подвигала" -> finVerb "MOVE" "PAST" A.f v ++ arg Instr P.Arg2 v
  "подобен" -> shortAdj A.m P.Quality "SIMILAR_TO" v ++ arg Dat P.Arg2 v
  "показались" -> raisingVerb "SEEM" "PAST" A.pl v ++ optional (arg Dat P.Experiencer v)
  "поливать" -> infinitive "TO_WATER" v ++ directObject v
  "полнота" -> nounSg Nom Fem "PLENITUDE" v ++ genHead P.Arg1 v
  "помидор" -> nounSg Nom Masc "TOMATO" v
  "помнит" -> finVerb "REMEMBER" "PRESENT" A.sg3 v ++ directObject v
  "помнят" -> finVerb "REMEMBER" "PRESENT" A.pl3 v ++ directObject v
  "помочь" -> infinitive "HELP" v ++ optional (arg Dat P.Arg2 v)
  "порядок" -> nounSg Acc Masc "ORDER" v ++ genHead P.Arg1 v
  "после" -> xor [[mite $ Argument ScalarAdverb v0, semT v0 "AFTER"] ++ optional (semPreposition Gen P.Anchor v),
                  adverb P.RelTime "AFTER" v ++ semPreposition Gen P.Anchor v]
  "потом" -> xor [[mite $ Argument ScalarAdverb v0, semT v0 "NEXT"], adverb P.RelTime "AFTER" v]
  "потому" -> [mite $ TwoWordCxt "потому что" True [ReasonComp v0 False] v0]
  "приуныли" -> finVerb "GET_SAD" "PAST" A.pl v
  "про" -> preposition "pro" Acc v
  "продавец" -> nounSg Nom Masc "SALESPERSON" v ++ genHead P.Place v
  "продавцом" -> nounSg Instr Masc "SALESPERSON" v ++ genHead P.Place v
  "просто" -> xor [modifierAdverb "JUST" v, adverb P.Manner "JUST" v]
  "пошел" -> finVerb "GO" "PAST" A.m v ++ [mite $ SemArgHead Obligatory Direction v0]
  "пошёл" -> finVerb "GO" "PAST" A.m v ++ [mite $ SemArgHead Obligatory Direction v0]
  "пошла" -> finVerb "GO" "PAST" A.f v ++ [mite $ SemArgHead Obligatory Direction v0]
  "пошли" -> finVerb "GO" "PAST" A.pl v ++ [mite $ SemArgHead Obligatory Direction v0]
  "пятая" -> adj Nom A.f P.Order "5" v
  "приехал" -> finVerb "ARRIVE" "PAST" A.m v
  "работает" -> finVerb "WORK" "PRESENT" A.sg3 v
  "работу" -> nounSg Acc Fem "WORK" v
  "работы" -> nounSg Gen Fem "WORK" v
  "радостью" -> nounSg Instr Fem "JOY" v
  "разбившуюся" -> adj Acc A.f P.Quality "SMASHED" v
  "разбилась" -> finVerb "SMASH" "PAST" A.f v
  "разбился" -> finVerb "SMASH" "PAST" A.m v
  "разошлись" -> finVerb "DISPERSE" "PAST" A.pl v ++ arg (PP "po" Dat) P.Goal v
  "раньше" -> xor [[mite $ Argument ScalarAdverb v0, semT v0 "EARLIER"], adverb P.RelTime "BEFORE" v] ++ optional (semPreposition Gen P.Anchor v)
  "ребенок" -> nounSg Nom Masc "CHILD" v
  "речь" -> nounSg Nom Fem "SPEECH" v ++ genHead P.Arg1 v
  "рта" -> nounSg Gen Masc "MOUTH" v
  "рынок" -> nounSg Acc Masc "MARKET" v
  "с" -> xor [preposition "s" Instr v, preposition "s" Gen v]
  "сад" -> nounSg Acc Masc "GARDEN" v
  "сада" -> nounSg Gen Masc "GARDEN" v
  "свалился" -> finVerb "FALL" "PAST" A.m v ++ optional (arg (PP "s" Gen) P.Source v)
  "свет" -> nounSg Nom Masc "LIGHT" v
  "своим" -> possessive Dat A.pl "SELF" v ++ [mite $ ReflexiveReference v0]
  "своими" -> possessive Instr A.pl "SELF" v ++ [mite $ ReflexiveReference v0]
  "себе" -> pronoun Dat A.empty "SELF" v ++ [mite $ ReflexiveReference v0]
  "сегодня" -> adverb P.RelTime "TODAY" v
  "семи" -> wordNumber Gen "7" v
  "семь" -> xor [wordNumber Nom "7" v, wordNumber Acc "7" v]
  "семьи" -> nounSg Gen Fem "FAMILY" v ++ genHead P.Arg1 v
  "семью" -> xor [nounSg Acc Fem "FAMILY" v ++ genHead P.Arg1 v, wordNumber Instr "7" v]
  "семья" -> nounSg Nom Fem "FAMILY" v ++ genHead P.Arg1 v
  "сестра" -> nounSg Nom Fem "SISTER" v ++ genHead P.Arg1 v
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
  "слепому" -> adj Dat A.m P.Quality "BLIND" v
  "слова" -> xor [nounPl Nom "WORDS" v, nounPl Acc "WORDS" v] ++ genHead P.Author v
  "словам" -> nounPl Dat "WORDS" v ++ genHead P.Author v
  "сломал" -> finVerb "BREAK" "PAST" A.m v ++ directObject v ++ arg Dat P.Receiver v
  "сломала" -> finVerb "BREAK" "PAST" A.f v ++ directObject v ++ arg Dat P.Receiver v
  "случай" -> nounSg Nom Masc "CASE" v
  "случае" -> nounSg Prep Masc "CASE" v ++ [mite $ ConditionCompHead v0]
  "случился" -> finVerb "HAPPEN" "PAST" A.m v ++ arg (PP "s" Instr) P.Experiencer v
  "смотреть" -> infinitive "LOOK" v ++ [mite $ SemArgHead Optional Direction (v "")]
  "смотрит" -> finVerb "LOOK" "PRESENT" A.sg3 v ++ [mite $ SemArgHead Optional Direction (v "")]
  "смысла" -> nounSg Gen Masc "MEANING" v
  "снег" -> nounSg Nom Masc "SNOW" v
  "со" -> xor [preposition "s" Instr v, preposition "s" Gen v]
  "сонет" -> xor [nounSg Nom Masc "SONNET" v, nounSg Acc Masc "SONNET" v]
  "соседа" -> nounSg Gen Masc "NEIGHBOR" v
  "соседей" -> xor [nounPl Acc "NEIGHBORS" v, nounPl Gen "NEIGHBORS" v]
  "соседям" -> nounPl Dat "NEIGHBORS" v
  "специалист" -> nounSg Nom Masc "SPECIALIST" v
  "спора" -> nounSg Gen Masc "ARGUE" v ++ genHead P.Arg1 v
  "спорили" -> finVerb "ARGUE" "PAST" A.pl v
  "спорить" -> infinitive "ARGUE" v
  "спросил" -> finVerb "ASK" "PAST" A.m v ++ optional (directObject v) ++ optional (xor [compHead P.Topic v, arg (PP "o" Prep) P.Topic v, arg (PP "pro" Acc) P.Topic v])
  "спросили" -> finVerb "ASK" "PAST" A.pl v ++ optional (directObject v) ++ optional (xor [compHead P.Topic v, arg (PP "o" Prep) P.Topic v, arg (PP "pro" Acc) P.Topic v])
  "спросить" -> infinitive "ASK" v ++ optional (directObject v) ++ optional (xor [compHead P.Topic v, arg (PP "o" Prep) P.Topic v])
  "стала" -> finVerb "BEGIN" "PAST" A.f v ++ [mite $ Control (v "theme"), semV v0 P.Theme (v "theme")]
  "стали" -> finVerb "BEGIN" "PAST" A.pl v ++ [mite $ Control (v "theme"), semV v0 P.Theme (v "theme")]
  "старик" -> nounSg Nom Masc "OLD_MAN" v
  "старуха" -> nounSg Nom Fem "OLD_LADY" v
  "старухи" -> nounPl Nom "OLD_LADIES" v
  "старуху" -> nounSg Acc Fem "OLD_LADY" v
  "счастию" -> nounSg Dat Neu "LUCK" v
  "счета" -> nounSg Gen Masc "COUNTING" v
  "счете" -> nounSg Prep Masc "COUNTING" v
  "считать" -> infinitive "COUNT" v ++ directObject v
  "та" -> adj Nom A.f P.Determiner "THAT" v
  "так" -> [mite $ TwoWordCxt "так как" True [ReasonComp v0 False] v0]
  "такой" -> adj Nom A.m P.Determiner "SUCH" v
  "там" -> adverb P.Location "THERE" v
  "танцевать" -> infinitive "DANCE" v
  "тебя" -> xor [pronoun Acc A.sg "YOU" v, pronoun Gen A.sg "YOU" v]
  "тоже" -> sAdverb P.Also "true" v
  "только" -> modifierAdverb "ONLY" v
  "том" -> adj Prep A.sg P.Determiner "THAT" v
  "тот" -> adj Nom A.m P.Determiner "THAT" v
  "третья" -> adj Nom A.f P.Order "3" v
  "три" -> wordNumber Acc "3" v
  "тут" -> sAdverb P.Emphasis "true" v
  "у" -> preposition "u" Gen v
  "увидел" -> finVerb "SEE" "PAST" A.m v ++ directObject v ++ optional (arg Instr P.Instrument v)
  "удивительный" -> adj Nom A.m P.Property "AMAZING" v
  "углу" -> nounSg Prep Masc "CORNER" v ++ genHead P.Arg1 v
  "удивление" -> nounSg Nom Neu "AMAZE" v ++ genHead P.Arg1 v
  "уж" -> sAdverb P.SAnchor "ALREADY" v
  "уже" -> sAdverb P.SAnchor "ALREADY" v
  "улиц" -> nounPl Gen "STREETS" v
  "улицы" -> nounSg Gen Fem "STREET" v
  "улыбнулась" -> finVerb "SMILE" "PAST" A.f v
  "умён" -> shortAdj A.m P.Quality "CLEVER" v
  "умная" -> adj Nom A.f P.Quality "CLEVER" v
  "умнее" -> comparativeAdj P.Quality "CLEVER" v
  "умной" -> adj Instr A.f P.Quality "CLEVER" v
  "умны" -> shortAdj A.pl P.Quality "CLEVER" v
  "умные" -> adj Nom A.pl P.Quality "CLEVER" v
  "умный" -> adj Nom A.m P.Quality "CLEVER" v
  "умным" -> adj Instr A.m P.Quality "CLEVER" v
  "упал" -> finVerb "FALL" "PAST" A.m v
  "упала" -> finVerb "FALL" "PAST" A.f v
  "флюсу" -> nounSg Dat Masc "GUMBOIL" v
  "фонтан" -> nounSg Nom Masc "FOUNTAIN" v
  "хитрец" -> nounSg Nom Masc "CUNNING_PERSON" v
  "челюсти" -> xor[nounSg Gen Fem "JAW" v, nounPl Nom "JAWS" v, nounPl Acc "JAWS" v]
  "челюсть" -> nounSg Acc Fem "JAW" v
  "челюстью" -> nounSg Instr Fem "JAW" v
  "чем" -> xor [caseWhWord Instr A.n v, caseWhWord Prep A.n v, [mite $ ComparisonAnchor Unsatisfied v0]]
  "чём" -> caseWhWord Prep A.n v
  "четвертая" -> adj Nom A.f P.Order "4" v
  "чрезмерного" -> adj Gen A.n P.Size "EXCESSIVE" v
  "что" -> xor [caseWhWord Nom A.n3 v, caseWhWord Acc A.n3 v, [mite $ Complementizer v0], [mite $ TwoWordCxt "потому что" False [] v0], [mite $ Relativizer v0, semT v0 "wh"]]
  "чьему" -> whWord A.empty v ++ animate v ++ xor [[mite $ Possessive Dat A.m v0], [mite $ Possessive Dat A.n v0]]
  "чьим" -> whWord A.empty v ++ animate v ++ [mite $ Possessive Dat A.pl v0]
  "шаль" -> xor [nounSg Nom Fem "SHAWL" v, nounSg Acc Fem "SHAWL" v]
  "шестая" -> adj Nom A.f P.Order "6" v
  "шести" -> wordNumber Gen "6" v
  "шел" -> xor [finVerb "GO" "PAST" A.m v ++ go_args v,
                 finVerb "WEATHER_BE" "PAST" A.m v,
                 finVerb "COME_SCALARLY" "PAST" A.m v ++ arg ScalarAdverb P.Order v] ++ [semS v0 P.Imperfective "true"]
  "шёл" -> xor [finVerb "GO" "PAST" A.m v ++ go_args v,
                 finVerb "WEATHER_BE" "PAST" A.m v,
                 finVerb "COME_SCALARLY" "PAST" A.m v ++ arg ScalarAdverb P.Order v] ++ [semS v0 P.Imperfective "true"]
  "эта" -> adj Nom A.f P.Determiner "THIS" v
  "это" -> xor [pronoun Nom (A.Agr (Just A.Neu) (Just A.Sg) $ Just 3) "THIS" v, pronoun Acc (A.Agr (Just A.Neu) (Just A.Sg) $ Just 3) "THIS" v, [mite $ Word v0 word]]
  "этот" -> adj Nom A.m P.Determiner "THIS" v
  "этому" -> adj Dat A.sg P.Determiner "THIS" v
  "я" -> pronoun Nom (A.Agr Nothing (Just A.Sg) $ Just 1) "ME" v
  ":" -> xor [[mite $ Colon "directSpeech" v0], [mite $ Colon "elaboration" v0]]
  "-" -> xor [[mite $ SurroundingDash v0],
              [mite $ DirectSpeechDash v0],
              [mite $ Ellipsis v0 Nothing, semS v0 P.Elided "true"]
             ]
  "," -> xor [[mite $ SurroundingComma v0], conjunction v0 "," True]
  "\"" -> xor [[mite $ Quote v0 True], [mite $ Quote v0 False]]
  _ ->
    if "ой" `isSuffixOf` word then 
      let nomName = take (length word - 2) word ++ "ая" in
      [semS v0 P.Name nomName] ++
      xor [adj Gen A.f P.VName "name" v,
           nounSg Gen Fem "STREET" (makeV v0 "noun") ++ [semV (v "noun") P.VName v0]]
    else if "ий" `isSuffixOf` word then adj Acc A.m P.VName "name" v ++ [semS v0 P.Name word]
    else [mite $ Word v0 word]
