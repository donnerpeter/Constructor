module Constructor.Tests.SonnetVariations where
import Constructor.Tests.Testing
import Test.HUnit

var1 = TestLabel "sonnetVariations 1" $ TestList [
--  translateTest "Я вдруг забыл, что идет дальше." 
--          "I suddenly forgot what comes next."
--  ,
--  translateTest "Я вдруг забыл, что - 7 или 8 - идет раньше" "I suddenly forgot what comes first - 7 or 8"
--  ,
--  translateTest "Я вдруг забыл, что, 7 или 8, идет раньше" "I suddenly forgot what comes first - 7 or 8"
--  ,
--  translateTest "Я вдруг забыл, кого я видел - кассиршу или соседей" 
--         "I suddenly forgot who I saw, the cashier or my neighbors"
--  ,
  -- 1_789_Translation
  translateTest "Я забыл, что идёт раньше - 7, 8 или 9."
                "I forgot what comes first - 7, 8 or 9."
  ,
  -- 1_ForgetFact
  translateTest "Я забыл, что 7 идёт раньше 8."
                "I forgot, that a 7 comes before an 8."
  ,
  -- 1_Forget2Facts
  translateTest "Я забыл, что 7 идёт раньше 8 и что 4 идёт после 3."
                "I forgot, that a 7 comes before an 8 and that a 4 comes after a 3."
  ,
  -- 1_ForgetRecursive
  translateTest "Я забыл, что я это забыл"
                "I forgot, that I forgot that"
  ,
  -- 1_ForgetRecursiveVO
  translateTest "Я забыл, что я забыл это"
                "I forgot, that I forgot that"
  ,
  -- 1_ForgetRecursive2
  translateTest "Я забыл, что я забыл"
                "I forgot what I forgot"
  ,
  -- 1_ForgetRecursive3Fact
  translateTest "Я забыл, что я забыл, что 7 идет раньше 8"
                "I forgot, that I forgot, that a 7 comes before an 8"
  ,
  -- 1_ForgetWhatIDid
  translateTest "Я забыл, что я делал."
                "I forgot what I did."
  ,
  -- 1_ForgetRecursive3Question
  translateTest "Я забыл, что я забыл, что идет после 8"
                "I forgot, that I forgot what comes after an 8"
  ,
  -- 1_ForgetRecursiveEnumeration
  translateTest "Я забыл, что я делал, что идет после 8 и что идет раньше 7."
                "I forgot what I did, what comes after an 8, and what comes before a 7."
  ,
  -- 1_ForgetRecursive2Questions
  translateTest "Я забыл, что я забыл, что идет после 8 и что идет раньше 7"
                "I forgot, that I forgot what comes after an 8 and what comes before a 7"
  ,
  -- 1_Forget2Questions
  translateTest "Я забыл, что идёт после 7 и что идёт раньше 8."
                "I forgot what comes after a 7 and what comes before an 8."
  ,
  -- 1_ForgetQuestionAndFact
  translateTest "Я забыл, что 8 идёт после 7 и что идёт раньше 5."
                "I forgot, that an 8 comes after a 7 and what comes before a 5."
  ,
  -- 1_ForgetFactAndQuestion
  translateTest "Я забыл, что идёт после 7 и что 4 идёт раньше 5."
                "I forgot what comes after a 7 and that a 4 comes before a 5."
 ]

var2 = TestLabel "sonnetVariations 2" $ TestList [
  -- 2_ThinkingNotSitting
  translateTest "Я забыл, что они думают, а не просто сидят."
                "I forgot, that they were thinking and not just sitting."
  ,
  -- 2_AskWhatTheySaid
  translateTest "Я спросил, что они сказали."
                "I asked what they said."
  ,
  -- 2_WhatThinkNotWhatSaid
  translateTest "Я спросил, что они думают, а не что они сказали."
                "I asked what they were thinking and not what they said."
  ,
  -- 2_ThinkingNotSitting_He
  translateTest "Я забыл, что он думает, а не просто сидит."
                "I forgot, that he was thinking and not just sitting."
  ,
  -- 2_NotSittingThinking
  translateTest "Я забыл, что они не просто сидят, а думают."
                "I forgot, that they were not just sitting, but thinking."
  ,
  -- 2_NotThinkingSitting
  translateTest "Я забыл, что они не думают, а просто сидят."
                "I forgot, that they were not thinking, but just sitting."
  ,
  translateTest "Он отправился к соседям" 
                "He went to his neighbors"
  ,
  translateTest "Я спросил их, что они думают по этому поводу" 
                "I asked them about their opinion on this matter"
 ]

var3 = TestLabel "sonnetVariations 3" $ TestList [
  -- 3_My
  translateTest "Каково же было мое удивление"
                "Great was my amazement"
  ,
  -- 3_She
  translateTest "Она вдруг обнаружила, что не может вспомнить порядок счета"
                "She suddenly discovered, that she couldn't recall the counting order"
  ,
  -- 3_Short
  translateTest "Каково же было их и мое удивление"
                "Great was their and my amazement"
  ,
  -- 3_Their
  translateTest "Каково же было их удивление"
                "Great was their amazement"
  ,
  -- 3_He
  translateTest "Он вдруг обнаружил, что не может вспомнить порядок счета"
                "He suddenly discovered, that he couldn't recall the counting order"
 ]

var4 = TestLabel "sonnetVariations 4" $ TestList [
  -- 4_TheyRemember123
  translateTest "Они помнят 1, 2 и 3"
                "They remember 1, 2 and 3"
  ,
  -- 4_TheyRemember78
  translateTest "Они помнят семь и восемь"
                "They remember a seven and an eight"
  ,
  -- 4_4
  translateTest "2 и 3 они помнят, а 4 забыли"
                "They remember 2 and 3, but forgot 4"
  ,
  -- 4_45
  translateTest "2 и 3 они помнят, а 4 и 5 забыли"
                "They remember 2 and 3, but forgot 4 and 5"
  ,
  -- 4_TheyForgot45
  translateTest "Они забыли 4 и 5"
                "They forgot 4 and 5"
  ,
  -- 4_123TheyRemember
  translateTest "1, 2 и 3 они помнят"
                "They remember 1, 2 and 3"
  ,
  -- 4_HeRemembers
  translateTest "1, 2 и 3 помнит, а дальше забыл"
                "He remembers 1, 2 and 3, but forgot what comes next"
  ,
  -- 4_SheRemembers
  translateTest "1, 2 и 3 помнит, а дальше забыла"
                "She remembers 1, 2 and 3, but forgot what comes next"
 ]

var5 = TestLabel "sonnetVariations 5" $ TestList [
  -- 5_OnTheCorner
  translateTest "Мы все пошли в коммерческий магазин \"Гастроном\" на углу и спросили кассиршу о нашем недоумении"
                "We all went to a commercial grocery store on the corner to consult a cashier on our predicament"
  ,
  -- 5_OnTheCornerOfStreets
  translateTest "Мы все пошли в коммерческий магазин \"Гастроном\", что на углу Знаменской и Бассейной улиц, и спросили кассиршу о нашем недоумении"
                "We all went to a commercial grocery store, the one that's on the corner of Znamenskaya and Basseinaya streets to consult a cashier on our predicament"
  ,
  translateTest "Мы все пошли в коммерческий магазин \"Гастроном\", что на углу Театральной, Знаменской и Бассейной улиц, и спросили кассиршу о нашем недоумении"
                "We all went to a commercial grocery store, the one that's on the corner of Teatralnaya, Znamenskaya and Basseinaya streets to consult a cashier on our predicament"
  ,
  -- 5_Shorter
  translateTest "Мы пошли в магазин \"Гастроном\", что на углу, и спросили кассиршу о нашем недоумении"
                "We went to a grocery store, the one that's on the corner to consult a cashier on our predicament"
  ,
  -- 5_NonameStore
  translateTest "Мы пошли в магазин."
                "We went to a store."
  ,
  -- 5_OnTheCornerOf
  translateTest "Мы все пошли в коммерческий магазин \"Гастроном\" на углу Знаменской и Бассейной улицы и спросили кассиршу о нашем недоумении"
                "We all went to a commercial grocery store on the corner of Znamenskaya and Basseinaya streets to consult a cashier on our predicament"
  ,
  -- 5_ShopWasOnTheCorner
  translateTest "Магазин был на углу."
                "The store was on the corner."
  ,
  -- 5_TheCashier
  translateTest "Мы спросили кассиршу о нашем недоумении"
                "We consulted the cashier on our predicament"
 ]

var6 = TestLabel "sonnetVariations 6" $ TestList [
  -- 6_Shorter
  translateTest "Кассирша улыбнулась, вынула изо рта молоточек и сказала:"
                "The cashier gave us a smile, took a hammer out of her mouth, and said:"
  ,
  -- 6_1and3Clauses
  translateTest "Кассирша улыбнулась и сказала:"
                "The cashier gave us a smile and said:"
  ,
  -- 6_2and3Clauses
  translateTest "Кассирша вынула изо рта молоточек и, подвигав носом, сказала:"
                "The cashier took a hammer out of her mouth, and moving her nose back and forth, she said:"
  ,
  -- 6_2and3Clauses_He
  translateTest "Кассир вынул изо рта молоточек и, подвигав носом, сказал:"
                "The cashier took a hammer out of his mouth, and moving his nose back and forth, he said:"
  ,
  -- 6_WithoutBackgrounded
  translateTest "Кассирша грустно улыбнулась, слегка подвигала носом и сказала:"
                "The cashier gave us a sad smile, moved her nose slightly back and forth, and said:"
  ,
  -- 6_3ClauseOnly
  translateTest "Кассирша, слегка подвигав носом, сказала:"
                "Moving her nose slightly back and forth, the cashier said:"
  ,
  -- 6_MaleCashier
  translateTest "Кассир, слегка подвигав носом, сказал:"
                "Moving his nose slightly back and forth, the cashier said:"
  ,
  -- 6_Jaw
  translateTest "Кассирша, слегка подвигав челюстью, сказала:"
                "Moving her jaw slightly back and forth, the cashier said:"
 ]

var67 = TestLabel "sonnetVariations 6+7" $ TestList [
  -- 6_7_DirectNoConditions
  translateTest "Кассирша сказала, подвигав носом:\n\
\ - Семь идет после восьми."
                "The cashier said, moving her nose back and forth:\n\
\- A seven comes after an eight."
  ,
  -- 6_7_IndirectHeOpinion
  translateTest "Кассир сказал, что, по его мнению, семь идет после восьми в том случае, когда восемь идет после семи"
                "The cashier said that in his opinion, a seven comes after an eight, only if an eight comes after a seven"
  ,
  -- 6_7_IndirectHeSevenOpinion
  translateTest "Кассир сказал, что семь, по его мнению, идет после восьми в том случае, когда восемь идет после семи"
                "The cashier said that in his opinion, a seven comes after an eight, only if an eight comes after a seven"
  ,
  -- 6_7_IndirectHe
  translateTest "Кассир сказал, что семь идет после восьми в том случае, когда восемь идет после семи"
                "The cashier said that a seven comes after an eight, only if an eight comes after a seven"
  ,
  -- 6_7_IndirectHeShort
  translateTest "Кассир сказал, что семь идет после восьми"
                "The cashier said that a seven comes after an eight"
  ,
  -- 6_7_IndirectSheShort
  translateTest "Кассирша сказала, что семь идет после восьми"
                "The cashier said that a seven comes after an eight"
  ,
  -- 6_7_IndirectSheShortConjunct
  translateTest "Кассирша сказала, что семь идет после восьми, и грустно улыбнулась"
                "The cashier said that a seven comes after an eight and gave us a sad smile"
  ,
  -- 6_7_IndirectNoOpinion
  translateTest "Кассирша сказала, что семь идет после восьми в том случае, когда восемь идет после семи"
                "The cashier said that a seven comes after an eight, only if an eight comes after a seven"
  ,
  -- 6_7_IndirectShe
  translateTest "Кассирша сказала, что, по ее мнению, семь идет после восьми в том случае, когда восемь идет после семи"
                "The cashier said that in her opinion, a seven comes after an eight, only if an eight comes after a seven"
  ,
  -- 6_7_IndirectSheMovingNose
  translateTest "Кассирша сказала, подвигав носом, что, по ее мнению, семь идет после восьми в том случае, когда восемь идет после семи"
                "The cashier said, moving her nose back and forth, that in her opinion, a seven comes after an eight, only if an eight comes after a seven"
  ,
  -- 6_7_IndirectHeWhatOpinion
  translateTest "Кассир сказал нам, что, по его мнению, идет после восьми"
                "The cashier told us, what, in his opinion, comes after an eight"
  ,
  -- 6_7_IndirectHeWhat
  translateTest "Кассир сказал нам, что идет после восьми"
                "The cashier told us what comes after an eight"
 ]

var7 = TestLabel "sonnetVariations 7" $ TestList [
  -- 7_IMO_AfterSubject
  translateTest "Семь, по-моему, идет после восьми в том случае, когда восемь идет после семи"
                "In my opinion, a seven comes after an eight, only if an eight comes after a seven"
  ,
  -- 7_IMO_End1Clause
  translateTest "Семь идет после восьми в том случае, по-моему, когда восемь идет после семи"
                "In my opinion, a seven comes after an eight, only if an eight comes after a seven"
  ,
  -- 7_Earlier
  translateTest "Семь идет раньше восьми в том случае, когда восемь идет раньше семи"
                "A seven comes before an eight, only if an eight comes before a seven"
  ,
  -- 7_If
  translateTest "Семь идет раньше восьми в случае, если восемь идет раньше семи"
                "A seven comes before an eight, only if an eight comes before a seven"
  ,
  -- 7_JustCaseNotThat
  translateTest "Семь идет раньше восьми в случае, когда восемь идет раньше семи"
                "A seven comes before an eight, only if an eight comes before a seven"
  ,
  -- 7_NoCaseWhen
  translateTest "Семь идет раньше восьми, когда восемь идет раньше семи"
                "A seven comes before an eight, when an eight comes before a seven"
  ,
  -- 7_NoCaseIf
  translateTest "Семь идет раньше восьми, если восемь идет раньше семи"
                "A seven comes before an eight, if an eight comes before a seven"
 ]

var8 = TestLabel "sonnetVariations 8" $ TestList [
  -- 8_GreatJoy
  translateTest "Мы поблагодарили кассиршу и с большой радостью выбежали из магазина"
                "We thanked the cashier and ran out of the store with great joy"
  ,
  -- 8_Relieved
  translateTest "Мы поблагодарили кассиршу и с облегчением выбежали из магазина"
                "We thanked the cashier and ran out of the store with relief"
  ,
  -- 8_GreatRelieve
  translateTest "Мы поблагодарили кассиршу и с большим облегчением выбежали из магазина"
                "We thanked the cashier and ran out of the store with great relief"
  ,
  -- 8_Her
  translateTest "Мы поблагодарили её и с радостью выбежали из магазина"
                "We thanked her and ran cheerfully out of the store"
  ,
  -- 8_HerPostfixJoy
  translateTest "Мы поблагодарили ее и выбежали из магазина с радостью"
                "We thanked her and ran out of the store cheerfully"
 ]

var9 = TestLabel "sonnetVariations 9" $ TestList [
  -- 9_Short1
  translateTest "Но тут мы опять приуныли"
                "But there we got sad again"
  ,
  -- 9_Short1Then
  translateTest "Но потом мы опять приуныли"
                "But then we got sad again"
  ,
  -- 9_ThenNotBut
  translateTest "Потом мы опять приуныли"
                "Then we got sad again"
  ,
  -- 9_ThereNotBut
  translateTest "Тут мы опять приуныли"
                "There we got sad again"
  ,
  -- 9_ThinkingGotSad
  translateTest "Вдумываясь в слова кассирши, мы опять приуныли"
                "Thinking carefully about cashier's words, we got sad again"
  ,
  -- 9_GotSadThinking
  translateTest "Мы опять приуныли, вдумываясь в слова кассирши"
                "We got sad again, thinking carefully about cashier's words"
  ,
  -- 9_12JustMeaning
  translateTest "Мы приуныли, так как ее слова нам показались лишенными смысла"
                "We got sad because her words were void of meaning"
  ,
  -- 9_12Meaningless
  translateTest "Мы приуныли, так как ее слова нам показались бессмысленными"
                "We got sad because her words were meaningless"
  ,
  -- 9_2Only
  translateTest "Ее слова нам показались лишенными всякого смысла"
                "Her words seemed void of any meaning to us"
  ,
  -- 9_2NoUs
  translateTest "Ее слова показались лишенными смысла"
                "Her words seemed void of meaning"
  ,
  -- 9_2Meaningless
  translateTest "Ее слова нам показались бессмысленными"
                "Her words seemed meaningless to us"
  ,
  -- 9_2Smart
  translateTest "Ее речь нам кажется умной"
                "Her speech seems clever to us."
  ,
  -- 9_SmartUsPrefix
  translateTest "Нам ее речь кажется умной."
                "To us, her speech seems clever."
  ,
  -- 9_GotSadThinkingNeighbors
  translateTest "Мы опять приуныли, вдумываясь в слова соседей"
                "We got sad again, thinking carefully about neighbours' words"
  ,
  -- 9_GotSadThinkingNeighbor
  translateTest "Мы опять приуныли, вдумываясь в слова соседа"
                "We got sad again, thinking carefully about neighbour's words"
  ,
  -- 9_12WePrefix
  translateTest "Мы приуныли, так как нам ее слова показались лишенными всякого смысла"
                "We got sad because her words were void of any meaning"
  ,
  -- 9_12WePrefixBecause
  translateTest "Мы приуныли, потому что ее слова показались нам лишенными всякого смысла"
                "We got sad because her words were void of any meaning"
  ,
  -- 9_2WePrefix
  translateTest "Нам ее слова показались лишенными всякого смысла"
                "To us, her words seemed void of any meaning"
  ,
  -- 9_2JustMeaning
  translateTest "Ее слова нам показались лишенными смысла"
                "Her words seemed void of meaning to us"
 ]

var10 = TestLabel "sonnetVariations 10" $ TestList [
  -- 10_WhatToDo
  translateTest "Что делать?"
                "What to do?"
  ,
  -- 10_WhatToDo_Me
  translateTest "Что мне делать?"
                "What am I supposed to do?"
  ,
  -- 10_WhatToAsk
  translateTest "Что спросить?"
                "What to ask?"
  ,
  -- 10_WhatToAsk_Me
  translateTest "Что мне спросить?"
                "What am I supposed to ask?"
  ,
  -- 10_Me
  translateTest "Что мне было делать?"
                "What was I supposed to do?"
  ,
  -- 10_INeedPresent
  translateTest "Что мне нужно?"
                "What do I need?"
  ,
  -- 10_WhatDidIDo
  translateTest "Что я делал?"
                "What did I do?"
  ,
  -- 10_NothingToDoPast
  translateTest "Делать нам было нечего"
                "We had nothing to do"
  ,
  -- 10_NothingToDo
  translateTest "Делать нам нечего"
                "We have nothing to do"
  ,
  -- 10_SomethingToDo
  translateTest "Нам есть, что делать"
                "We have something to do"
  ,
  -- 10_INeedPast
  translateTest "Что мне было нужно?"
                "What did I need?"
  ,
  -- 10_WhatDidWeDo
  translateTest "Что мы делали?"
                "What did we do?"
  ,
  -- 10_Subordinate
  translateTest "Я спросил, что делать."
                "I asked what to do."
 ]

var11 = TestLabel "sonnetVariations 11" $ TestList [
  -- 11_WaterTrees
  translateTest "Мы стали поливать деревья"
                "We started watering trees"
  ,
  -- 11_Dance
  translateTest "Мы стали танцевать"
                "We started dancing"
  ,
  -- 11_JustGarden
  translateTest "Мы пошли в сад"
                "We went to the garden"
  ,
  -- 11_LittleGardenOnTheCorner
  translateTest "Мы пошли в маленький сад на углу"
                "We went to the small garden on the corner"
  ,
  -- 11_BigGardenOnTheCorner
  translateTest "Мы пошли в большой сад, что на углу Знаменской и Бассейной улиц"
                "We went to the big garden, the one that's on the corner of Znamenskaya and Basseinaya streets"
  ,
  -- 11_CountMouney
  translateTest "Мы стали считать деньги"
                "We started counting money"
 ]

var12 = TestLabel "sonnetVariations 12" $ TestList [
  -- 12_StartedCounting
  translateTest "Мы начали считать деревья"
                "We started counting trees"
  ,
  -- 12_ReachingGardenStartedCounting
  translateTest "Дойдя до сада, мы начали считать деревья"
                "Reaching the garden, we started counting trees"
  ,
  -- 12_Reaching5
  translateTest "Дойдя до 5-ти, мы начали спорить"
                "Reaching a five, we started arguing"
  ,
  -- 12_ButReachingFive
  translateTest "Но, дойдя до 5, мы начали спорить"
                "But reaching a 5, we started arguing"
  ,
  -- 12_ReachingFivePostfix
  translateTest "Но мы начали спорить, дойдя до 5"
                "But we started arguing, reaching a 5"
  ,
  -- 12_ReachingSixInLetters
  translateTest "Дойдя до шести, мы начали спорить"
                "Reaching a six, we started arguing"
  ,
  -- 12_MyTheirOpinion
  translateTest "По моему мнению, раньше идет 7, а по их мнению - 8"
                "In my opinion, a 7 comes first; but in their opinion an 8 does"
  ,
  -- 12_HumbleOpinions
  translateTest "По моему скромному мнению, раньше идет 7, а по их скромному мнению - 8"
                "In my humble opinion, a 7 comes first; but in their humble opinion an 8 does"
  ,
  -- 12_ImhoVsCashier
  translateTest "По-моему, раньше идет 7, а по мнению кассирши - 8"
                "In my opinion, a 7 comes first; but in the opinion of the cashier an 8 does"
  ,
  -- 12_ImhoVsCashierFront7
  translateTest "По-моему, 7 идет раньше, а по мнению кассирши - 8"
                "In my opinion, a 7 comes first; but in the opinion of the cashier an 8 does"
  ,
  -- 12_ImhoVsCashierLater
  translateTest "По-моему, 7 идет раньше, а по мнению кассирши - потом"
                "In my opinion, a 7 comes first; but in the opinion of the cashier, it comes next"
  ,
  -- 12_AccordingCashier
  translateTest "По словам кассирши, раньше идет 7"
                "According to the cashier, a 7 comes first"
  ,
  -- 12_7Next
  translateTest "По словам кассирши, потом идет 7"
                "According to the cashier, a 7 comes next"
  ,
  -- 12_ShortNoEllipsis
  translateTest "По мнению одних дальше следовало 7, по мнению других дальше следовало 8"
                "In the opinion of some, a 7 went next; but in opinion of others, an 8 went next"
  ,
  -- 12_Before7Followed
  translateTest "Раньше потом следовало 7"
                "Before, a 7 went next"
  ,
  -- 12_7After8
  translateTest "По словам кассирши, после 8 идет 7"
                "According to the cashier, a 7 comes after an 8"
  ,
  -- 12_7After6
  translateTest "После 6-ти идет 7"
                "A 7 comes after a six"
  ,
  -- 12_7WentAfter6
  translateTest "После 6 следовало 7"
                "A 7 went after a 6"
  ,
  -- 12_7Before8
  translateTest "7 идет раньше 8"
                "A 7 comes before an 8"
  ,
  -- 12_InOpinionCoordination
  translateTest "По мнению кассирши и по мнению других, семь идет раньше восьми"
                "In the opinion of the cashier and in opinion of others, a seven comes before an eight"
  ,
  -- 12_InOpinionCoordinationNumbers
  translateTest "По мнению кассирши и по мнению других, 7 идет раньше 8"
                "In the opinion of the cashier and in opinion of others, a 7 comes before an 8"
  ,
  -- 12_OpinionantCoordination
  translateTest "По мнению кассирши и других, 7 идет раньше 8"
                "In the opinion of the cashier and others, a 7 comes before an 8"
  ,
  -- 12_StoreCashier
  translateTest "По мнению кассирши магазина, семь идет раньше восьми"
                "In the opinion of the store cashier, a seven comes before an eight"
  ,
  -- 12_StoreCashierAndOthers
  translateTest "По мнению кассирши магазина и других, 7 идет раньше 8"
                "In the opinion of the store cashier and others, a 7 comes before an 8"
  ,
  -- 12_OpinionCoordinationWithWords
  translateTest "По словам кассирши и по мнению других, 7 идет раньше 8"
                "According to the cashier and in opinion of others, a 7 comes before an 8"
  ,
  -- 12_OpinionCoordinationWithWords_SamePerson_RepeatedPreposition
  translateTest "По словам и по мнению кассирши, 7 идет раньше 8"
                "According to and in the opinion of the cashier, a 7 comes before an 8"
  ,
  -- 12_OpinionCoordination
  translateTest "По мнению кассирши и мнению других, семь идет раньше восьми"
                "In the opinion of the cashier and opinion of others, a seven comes before an eight"
  ,
  -- 12_OpinionCoordinationNumbers
  translateTest "По мнению кассирши и мнению других, 7 идет раньше 8"
                "In the opinion of the cashier and opinion of others, a 7 comes before an 8"
  ,
  -- 12_OpinionCoordinationWithWords_SamePerson
  translateTest "По словам и мнению кассирши, семь идет раньше восьми"
                "According to and in the opinion of the cashier, a seven comes before an eight"
  ,
  -- 12_OpinionCoordinationWithWords_SamePersonNumbers
  translateTest "По словам и мнению кассирши, 7 идет раньше 8"
                "According to and in the opinion of the cashier, a 7 comes before an 8"
 ]

var13 = TestLabel "sonnetVariations 13" $ TestList [
  -- 13_LuckMissingNoCommas
  translateTest "Мы спорили бы очень долго, но по счастию тут со скамейки свалился какой-то ребенок и сломал себе обе челюсти"
                "We were arguing for a long time, when by some sheer luck, a child fell off a bench and broke both of his jaws"
  ,
  -- 13_LuckMissingComma1
  translateTest "Мы спорили бы очень долго, но по счастию, тут со скамейки свалился какой-то ребенок и сломал себе обе челюсти"
                "We were arguing for a long time, when by some sheer luck, a child fell off a bench and broke both of his jaws"
  ,
  -- 13_InnerClause
  translateTest "По счастию, тут со скамейки свалился какой-то ребенок"
                "By some sheer luck, a child fell off a bench"
  ,
  -- 13_InnerClauseLuckMissingComma2
  translateTest "По счастию тут со скамейки свалился какой-то ребенок"
                "By some sheer luck, a child fell off a bench"
  ,
  -- 13_OneJaw
  translateTest "Ребенок свалился и сломал себе челюсть"
                "The child fell and broke his jaw"
  ,
  -- 13_BothJawsAnd8Fingers
  translateTest "Ребенок сломал себе обе челюсти и 8 пальцев"
                "The child broke both of his jaws and 8 fingers"
  ,
  -- 13_BothJawsAndTwoFingers
  translateTest "Ребенок сломал себе обе челюсти и два пальца"
                "The child broke both of his jaws and two fingers"
  ,
  -- 13_ThreeJaws
  translateTest "Ребенок сломал себе 3 челюсти"
                "The child broke 3 of his jaws"
  ,
  -- 13_2Fingers
  translateTest "Ребенок сломал себе 2 пальца"
                "The child broke 2 of his fingers"
  ,
  -- 13_ThreeJawsAnd2Fingers
  translateTest "Ребенок сломал себе 3 челюсти и 2 пальца"
                "The child broke 3 of his jaws and 2 fingers"
  ,
  -- 13_2FingersAndAJaw
  translateTest "Ребенок сломал себе 2 пальца и челюсть"
                "The child broke 2 of his fingers and a jaw"
  ,
  -- 13_BothJawsAndOneFinger
  translateTest "Ребенок сломал себе обе челюсти и один палец"
                "The child broke both of his jaws and one finger"
  ,
  -- 13_BothJawsAndFinger
  translateTest "Ребенок сломал себе обе челюсти и палец"
                "The child broke both of his jaws and a finger"
  ,
  -- 13_MeFingers
  translateTest "Ребенок сломал мне три пальца"
                "The child broke three of my fingers"
  ,
  -- 13_FemaleCashierFingers
  translateTest "Кассирша сломала себе 8 пальцев"
                "The cashier broke 8 of her fingers"
  ,
  -- 13_MaleCashierFingers
  translateTest "Кассир сломал себе 2 пальца"
                "The cashier broke 2 of his fingers"
  ,
  -- 13_ManyJaws
  translateTest "Ребенок свалился и сломал себе челюсти"
                "The child fell and broke his jaws"
  ,
  -- 13_EightFingers
  translateTest "Ребенок сломал себе восемь пальцев"
                "The child broke eight of his fingers"
  ,
  -- 13_8Fingers
  translateTest "Ребенок сломал себе 8 пальцев"
                "The child broke 8 of his fingers"
  ,
  -- 13_BothJawsAndEightFingers
  translateTest "Ребенок сломал себе обе челюсти и восемь пальцев"
                "The child broke both of his jaws and eight fingers"
 ]

var14 = TestLabel "sonnetVariations 14" $ TestList [
  -- 14_Argument
  translateTest "Это отвлекло нас от спора"
                "That distracted us from the argument"
  ,
  -- 14_TheirArgument
  translateTest "Это отвлекло их от их спора"
                "That distracted them from their argument"
 ]

var15 = TestLabel "sonnetVariations 15" $ TestList [
  -- 15_AndThey
  translateTest "А они разошлись по домам."
                "And they all went home."
  ,
  -- 15_They
  translateTest "А потом они разошлись по домам."
                "And then they all went home."
  ,
  -- 15_ThenThey
  translateTest "Потом они разошлись по домам."
                "Then they all went home."
  ,
  -- 15_Rooms
  translateTest "А они разошлись по своим комнатам."
                "And they all went to their rooms."
  ,
  -- 15_RoomsApartments
  translateTest "А они разошлись по своим комнатам и квартирам"
                "And they all went to their rooms and apartments"
  ,
  -- 15_RoomsApartmentsOffices
  translateTest "А они разошлись по своим комнатам, квартирам и офисам."
                "And they all went to their rooms, apartments and offices."
  ,
  -- 15_TheirOffices
  translateTest "Мы разошлись по их офисам."
                "We all went to their offices."
 ]

varUnsorted = TestLabel "sonnetVariations unsorted" $ TestList [
  -- SheForgotThatGoesAfterVasya
  translateTest "Она забыла, что идет после Васи."
                "She forgot, that she went after Vasya."
  ,
  -- HeForgotThatBrokeHisJaw
  translateTest "Он забыл, что сломал себе челюсть."
                "He forgot, that he broke his jaw."
  ,
  -- HeForgotThatSheBrokeHisJaw
  translateTest "Он забыл, что она сломала ему челюсть."
                "He forgot, that she broke his jaw."
  ,
  -- SheForgotThatBrokeHerJaw
  translateTest "Она забыла, что сломала себе челюсть."
                "She forgot, that she broke her jaw."
  ,
  -- IAskedWhatToDoThem
  translateTest "Я спросил, что делать, их"
                "I asked them what to do"
  ,
  -- HeForgotThatGoesAfterVasya
  translateTest "Он забыл, что идет после Васи."
                "He forgot, that he went after Vasya."
  ,
  -- CashierGoesToTheShop
  translateTest "Кассирша идет в магазин"
                "The cashier goes to the store"
  ,
  -- AfterWorkCashierGoesToTheGarden
  translateTest "После работы кассирша идет в сад"
                "After the work, the cashier goes to the garden"
  ,
  -- CashierGoesToTheGardenAfterWork
  translateTest "Кассир идет в сад после работы"
                "The cashier goes to the garden after the work"
  ,
  -- AfterVasyaGoesCashier
  translateTest "После Васи идет кассир"
                "After Vasya, the cashier goes"
  ,
  -- CashierGoesAfterVasya
  translateTest "Кассир идет после Васи"
                "The cashier goes after Vasya"
 ]

sonnetVariations = [var1, var2, var3, var4, var5, var6, var7, var67, var8, var9, 
  var10, var11, var12, var13, var14, var15, varUnsorted]
  

