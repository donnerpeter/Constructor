package cons3

import junit.framework.Test
import junit.framework.TestCase
import junit.framework.TestSuite
import static cons3.SonnetTest.doParseTest
import static cons3.SonnetTest.doTranslateTest

/**
 * @author peter
 */
class SonnetVariationsTest extends TestCase {

  public void test3_Short() {
    doTranslateTest "Каково же было их и мое удивление",
                    "Great was their and my amazement"
  }

  public void test3_My() {
    doTranslateTest "Каково же было мое удивление",
                    "Great was my amazement"
  }
  public void test3_Their() {
    doTranslateTest "Каково же было их удивление",
                    "Great was their amazement"
  }

  public void test3_He() {
    doTranslateTest "Он вдруг обнаружил, что не может вспомнить порядок счета",
            "He suddenly discovered, that he couldn't recall the counting order"
  }
  public void test3_She() {
    doTranslateTest "Она вдруг обнаружила, что не может вспомнить порядок счета",
            "She suddenly discovered, that she couldn't recall the counting order"
  }

  public void testHeForgotThatGoesAfterVasya() {
    doTranslateTest "Он забыл, что идет после Васи.",
                    "He forgot, that he went after Vasya."
  }
  public void testSheForgotThatGoesAfterVasya() {
    doTranslateTest "Она забыла, что идет после Васи.",
                    "She forgot, that she went after Vasya."
  }
  public void testHeForgotThatBrokeHisJaw() {
    doTranslateTest "Он забыл, что сломал себе челюсть.",
                    "He forgot, that he broke his jaw."
  }
  public void testHeForgotThatSheBrokeHisJaw() {
    doTranslateTest "Он забыл, что она сломала ему челюсть.",
                    "He forgot, that she broke his jaw."
  }
  public void testSheForgotThatBrokeHerJaw() {
    doTranslateTest "Она забыла, что сломала себе челюсть.",
                    "She forgot, that she broke her jaw."
  }
  public void test2_ThinkingNotSitting() {
    doTranslateTest "Я забыл, что они думают, а не просто сидят.",
                    "I forgot, that they were thinking and not just sitting."
  }
  public void test2_AskWhatTheySaid() {
    doTranslateTest "Я спросил, что они сказали.",
                    "I asked what they said."
  }
  public void test2_WhatThinkNotWhatSaid() {
    doTranslateTest "Я спросил, что они думают, а не что они сказали.",
                    "I asked what they were thinking and not what they said."
  }
  public void test2_ThinkingNotSitting_He() {
    doTranslateTest "Я забыл, что он думает, а не просто сидит.",
                    "I forgot, that he was thinking and not just sitting."
  }
  public void test2_NotSittingThinking() {
    doTranslateTest "Я забыл, что они не просто сидят, а думают.",
                    "I forgot, that they were not just sitting, but thinking."
  }
  public void test2_NotThinkingSitting() {
    doTranslateTest "Я забыл, что они не думают, а просто сидят.",
                    "I forgot, that they were not thinking, but just sitting."
  }

  public void test1_789() {
    doParseTest "Я забыл, что идёт раньше - 7, 8 или 9", """
A.type=ME
B.type=FORGET
B.time=PAST
B.arg1=A
A.gender=masc
A.person=3
-- 2:
A.type=wh
B@1.arg2=B
C.time=PRESENT
C.arg1=A
A.person=3
B.content=C
B.type=question
B.questioned=A
C.type=COME_SCALARLY
C.order=EARLIER
C.anchor=D
-- 3:
A@2.variants=A
B.type=7
B.number=true
A.member=B
-- 4:
A.type=8
A.number=true
A@3.member=A
B.type=9
B.number=true
A@3.conj=or
A@3.member=B
"""
  }

  public void test1_789_Translation() {
    doTranslateTest "Я забыл, что идёт раньше - 7, 8 или 9.", "I forgot what comes first - 7, 8 or 9."
  }

  public void test1_ForgetFact() {
    doTranslateTest "Я забыл, что 7 идёт раньше 8.", "I forgot, that a 7 comes before an 8."
  }
  public void test1_Forget2Facts() {
    doTranslateTest "Я забыл, что 7 идёт раньше 8 и что 4 идёт после 3.", "I forgot, that a 7 comes before an 8 and that a 4 comes after a 3."
  }
  public void test1_ForgetRecursive() {
    doTranslateTest "Я забыл, что я это забыл", "I forgot, that I forgot that"
  }
  public void test1_ForgetRecursiveVO() {
    doTranslateTest "Я забыл, что я забыл это", "I forgot, that I forgot that"
  }
  public void test1_ForgetRecursive2() {
    doTranslateTest "Я забыл, что я забыл", "I forgot what I forgot"
  }
  public void test1_ForgetRecursive3Fact() {
    doTranslateTest "Я забыл, что я забыл, что 7 идет раньше 8", "I forgot, that I forgot, that a 7 comes before an 8"
  }
  public void test1_ForgetRecursive3Question() {
    doTranslateTest "Я забыл, что я забыл, что идет после 8", "I forgot, that I forgot what comes after an 8"
  }
  public void test1_ForgetWhatIDid() {
    doTranslateTest "Я забыл, что я делал.",
                    "I forgot what I did."
  }
  public void test1_ForgetRecursiveEnumeration() {
    doTranslateTest "Я забыл, что я делал, что идет после 8 и что идет раньше 7.",
                    "I forgot what I did, what comes after an 8, and what comes before a 7."
  }
  public void test1_ForgetRecursive2Questions() {
    doTranslateTest "Я забыл, что я забыл, что идет после 8 и что идет раньше 7",
                    "I forgot, that I forgot what comes after an 8 and what comes before a 7"
  }
  public void test1_Forget2Questions() {
    doTranslateTest "Я забыл, что идёт после 7 и что идёт раньше 8.", "I forgot what comes after a 7 and what comes before an 8."
  }

  public void test1_ForgetQuestionAndFact() {
    doTranslateTest "Я забыл, что 8 идёт после 7 и что идёт раньше 5.",
            "I forgot, that an 8 comes after a 7 and what comes before a 5."
  }
  public void test1_ForgetFactAndQuestion() {
    doTranslateTest "Я забыл, что идёт после 7 и что 4 идёт раньше 5.",
            "I forgot what comes after a 7 and that a 4 comes before a 5."
  }

  public void test4_123TheyRemember() {
    doTranslateTest "1, 2 и 3 они помнят", "They remember 1, 2 and 3"
  }
  public void test4_HeRemembers() {
    doTranslateTest "1, 2 и 3 помнит, а дальше забыл", "He remembers 1, 2 and 3, but forgot what comes next"
  }
  public void test4_SheRemembers() {
    doTranslateTest "1, 2 и 3 помнит, а дальше забыла", "She remembers 1, 2 and 3, but forgot what comes next"
  }

  public void test4_TheyRemember123() {
    doTranslateTest "Они помнят 1, 2 и 3", "They remember 1, 2 and 3"
  }

  public void test4_TheyRemember78() {
    doTranslateTest "Они помнят семь и восемь", "They remember a seven and an eight"
  }

  public void test4_4() {
    doTranslateTest "2 и 3 они помнят, а 4 забыли", "They remember 2 and 3, but forgot 4"
  }

  public void test4_45() {
    doTranslateTest "2 и 3 они помнят, а 4 и 5 забыли", "They remember 2 and 3, but forgot 4 and 5"
  }

  public void test4_TheyForgot45() {
    doTranslateTest "Они забыли 4 и 5", "They forgot 4 and 5"
  }

  public void test5_Shorter() {
    doTranslateTest 'Мы пошли в магазин "Гастроном", что на углу, и спросили кассиршу о нашем недоумении',
                    "We went to a grocery store, the one that's on the corner to consult a cashier on our predicament"
  }

  public void test5_NonameStore() {
    doTranslateTest 'Мы пошли в магазин.',
                    "We went to a store."
  }

  public void test5_OnTheCornerOf() {
    doTranslateTest 'Мы все пошли в коммерческий магазин "Гастроном" на углу Знаменской и Бассейной улицы и спросили кассиршу о нашем недоумении',
                    "We all went to a commercial grocery store on the corner of Znamenskaya and Basseinaya streets to consult a cashier on our predicament"
  }

  public void test5_OnTheCorner() {
    doTranslateTest 'Мы все пошли в коммерческий магазин "Гастроном" на углу и спросили кассиршу о нашем недоумении',
                    "We all went to a commercial grocery store on the corner to consult a cashier on our predicament"
  }

  public void test5_ShopWasOnTheCorner() {
    doTranslateTest 'Магазин был на углу.',
                    'The store was on the corner.'
  }

  public void test5_TheCashier() {
    doTranslateTest 'Мы спросили кассиршу о нашем недоумении',
                    "We consulted the cashier on our predicament"
  }

  public void test5_OnTheCornerOfStreets() {
    doTranslateTest 'Мы все пошли в коммерческий магазин "Гастроном", что на углу Знаменской и Бассейной улиц, и спросили кассиршу о нашем недоумении',
                    "We all went to a commercial grocery store, the one that's on the corner of Znamenskaya and Basseinaya streets to consult a cashier on our predicament"
  }


  public void test6_Shorter() {
    doTranslateTest 'Кассирша улыбнулась, вынула изо рта молоточек и сказала:',
                    "The cashier gave us a smile, took a hammer out of her mouth, and said:"
  }

  public void test6_1and3Clauses() {
    doTranslateTest 'Кассирша улыбнулась и сказала:',
                    "The cashier gave us a smile and said:"
  }

  public void test6_2and3Clauses() {
    doTranslateTest 'Кассирша вынула изо рта молоточек и, подвигав носом, сказала:',
                    "The cashier took a hammer out of her mouth, and moving her nose back and forth, she said:"
  }
  public void test6_2and3Clauses_He() {
    doTranslateTest 'Кассир вынул изо рта молоточек и, подвигав носом, сказал:',
                    "The cashier took a hammer out of his mouth, and moving his nose back and forth, he said:"
  }

  public void test6_WithoutBackgrounded() {
    doTranslateTest 'Кассирша грустно улыбнулась, слегка подвигала носом и сказала:',
                    "The cashier gave us a sad smile, moved her nose slightly back and forth, and said:"
  }

  public void test6_3ClauseOnly() {
    doTranslateTest 'Кассирша, слегка подвигав носом, сказала:',
                    "Moving her nose slightly back and forth, the cashier said:"
  }

  public void test6_MaleCashier() {
    doTranslateTest 'Кассир, слегка подвигав носом, сказал:',
                    "Moving his nose slightly back and forth, the cashier said:"
  }
  public void test6_Jaw() {
    doTranslateTest 'Кассирша, слегка подвигав челюстью, сказала:',
                    "Moving her jaw slightly back and forth, the cashier said:"
  }
  public void test6_7_IndirectSheShort() {
    doTranslateTest 'Кассирша сказала, что семь идет после восьми',
                    "The cashier said that a seven comes after an eight"
  }
  public void test6_7_IndirectSheShortConjunct() {
    doTranslateTest 'Кассирша сказала, что семь идет после восьми, и грустно улыбнулась',
                    "The cashier said that a seven comes after an eight and gave us a sad smile"
  }
  public void testIAskedWhatToDoThem() {
    doTranslateTest 'Я спросил, что делать, их',
                    "I asked them what to do"
  }
  public void test6_7_IndirectNoOpinion() {
    doTranslateTest 'Кассирша сказала, что семь идет после восьми в том случае, когда восемь идет после семи',
                    "The cashier said that a seven comes after an eight, only if an eight comes after a seven"
  }
  public void test6_7_IndirectShe() {
    doTranslateTest 'Кассирша сказала, что, по ее мнению, семь идет после восьми в том случае, когда восемь идет после семи',
                    "The cashier said that in her opinion, a seven comes after an eight, only if an eight comes after a seven"
  }
  public void test6_7_IndirectSheMovingNose() {
    doTranslateTest 'Кассирша сказала, подвигав носом, что, по ее мнению, семь идет после восьми в том случае, когда восемь идет после семи',
                    "The cashier said, moving her nose back and forth, that in her opinion, a seven comes after an eight, only if an eight comes after a seven"
  }
  public void test6_7_DirectNoConditions() {
    doTranslateTest 'Кассирша сказала, подвигав носом:\n - Семь идет после восьми.',
                    "The cashier said, moving her nose back and forth:\n- A seven comes after an eight."
  }
  public void _testWhoseOpinion() {
    doTranslateTest 'Я забыл, по чьему мнению семь идет после восьми.',
                    "I forgot in whose opinion a seven comes after an eight."
  }
  public void _testAccordingToWhom() {
    doTranslateTest 'Я забыл, по чьим словам семь идет после восьми.',
                    "I forgot according to whom a seven comes after an eight."
  }
  public void _testAccorgindToAndInOpinionOfWhomPossessive() {
    doTranslateTest 'Я забыл, по чьим словам и мнению семь идет после восьми.',
                    "I forgot according to and in the opinion of who a seven comes after an eight."
  }
  public void _testAccordingToWordsOfWho() {
    doTranslateTest 'Я забыл, по словам кого семь идет после восьми.',
                    "I forgot according to whom a seven comes after an eight."
  }
  public void _testAccorgindToAndInOpinionOfWhomGenitive() {
    doTranslateTest 'Я забыл, по словам и мнению кого семь идет после восьми.',
                    "I forgot according to and in the opinion of who a seven comes after an eight."
  }
  public void test6_7_IndirectHeOpinion() {
    doTranslateTest 'Кассир сказал, что, по его мнению, семь идет после восьми в том случае, когда восемь идет после семи',
                    "The cashier said that in his opinion, a seven comes after an eight, only if an eight comes after a seven"
  }
  public void test6_7_IndirectHeSevenOpinion() {
    doTranslateTest 'Кассир сказал, что семь, по его мнению, идет после восьми в том случае, когда восемь идет после семи',
                    "The cashier said that in his opinion, a seven comes after an eight, only if an eight comes after a seven"
  }
  public void test6_7_IndirectHe() {
    doTranslateTest 'Кассир сказал, что семь идет после восьми в том случае, когда восемь идет после семи',
                    "The cashier said that a seven comes after an eight, only if an eight comes after a seven"
  }
  public void test6_7_IndirectHeWhatOpinion() {
    doTranslateTest 'Кассир сказал нам, что, по его мнению, идет после восьми',
                    "The cashier told us, what, in his opinion, comes after an eight"
  }
  public void test6_7_IndirectHeWhat() {
    doTranslateTest 'Кассир сказал нам, что идет после восьми',
                    "The cashier told us what comes after an eight"
  }
  public void test6_7_IndirectHeShort() {
    doTranslateTest 'Кассир сказал, что семь идет после восьми',
                    "The cashier said that a seven comes after an eight"
  }

  public static Test suite() {
    final TestSuite suite = new TestSuite();
    for (i in 0..0) {
      suite.addTestSuite(SonnetVariationsTest.class);
    }
    return suite;
  }

  public void test7_IMO_AfterSubject() {
    doTranslateTest 'Семь, по-моему, идет после восьми в том случае, когда восемь идет после семи',
                    "In my opinion, a seven comes after an eight, only if an eight comes after a seven"
  }

  public void test7_IMO_End1Clause() {
    doTranslateTest 'Семь идет после восьми в том случае, по-моему, когда восемь идет после семи',
                    "In my opinion, a seven comes after an eight, only if an eight comes after a seven"
  }

  public void test7_Earlier() {
    doTranslateTest 'Семь идет раньше восьми в том случае, когда восемь идет раньше семи',
                    "A seven comes before an eight, only if an eight comes before a seven"
  }

  public void test7_JustCaseNotThat() {
    doTranslateTest 'Семь идет раньше восьми в случае, когда восемь идет раньше семи',
                    "A seven comes before an eight, only if an eight comes before a seven"
  }

  public void test7_If() {
    doTranslateTest 'Семь идет раньше восьми в случае, если восемь идет раньше семи',
                    "A seven comes before an eight, only if an eight comes before a seven"
  }

  public void test7_NoCaseWhen() {
    doTranslateTest 'Семь идет раньше восьми, когда восемь идет раньше семи',
                    "A seven comes before an eight, when an eight comes before a seven"
  }

  public void test7_NoCaseIf() {
    doTranslateTest 'Семь идет раньше восьми, если восемь идет раньше семи',
                    "A seven comes before an eight, if an eight comes before a seven"
  }

  public void test8_GreatJoy() {
    doTranslateTest 'Мы поблагодарили кассиршу и с большой радостью выбежали из магазина',
                    "We thanked the cashier and ran out of the store with great joy"
  }

  public void test8_Relieved() {
    doTranslateTest 'Мы поблагодарили кассиршу и с облегчением выбежали из магазина',
                    "We thanked the cashier and ran out of the store with relief"
  }

  public void test8_GreatRelieve() {
    doTranslateTest 'Мы поблагодарили кассиршу и с большим облегчением выбежали из магазина',
                    "We thanked the cashier and ran out of the store with great relief"
  }

  public void test8_Her() {
    doTranslateTest 'Мы поблагодарили её и с радостью выбежали из магазина',
                    "We thanked her and ran cheerfully out of the store"
  }

  public void test8_HerPostfixJoy() {
    doTranslateTest 'Мы поблагодарили ее и выбежали из магазина с радостью',
                    "We thanked her and ran out of the store cheerfully"
  }

  public void test9_Short1() {
    doTranslateTest 'Но тут мы опять приуныли',
                    "But there we got sad again"
  }
  public void test9_Short1Then() {
    doTranslateTest 'Но потом мы опять приуныли',
                    "But then we got sad again"
  }
  public void test9_ThenNotBut() {
    doTranslateTest 'Потом мы опять приуныли',
                    "Then we got sad again"
  }
  public void test9_ThereNotBut() {
    doTranslateTest 'Тут мы опять приуныли',
                    "There we got sad again"
  }

  public void test9_ThinkingGotSad() {
    doTranslateTest 'Вдумываясь в слова кассирши, мы опять приуныли',
                    "Thinking carefully about cashier's words, we got sad again"
  }
  public void test9_GotSadThinking() {
    doTranslateTest 'Мы опять приуныли, вдумываясь в слова кассирши',
                    "We got sad again, thinking carefully about cashier's words"
  }
  public void test9_GotSadThinkingNeighbors() {
    doTranslateTest 'Мы опять приуныли, вдумываясь в слова соседей',
                    "We got sad again, thinking carefully about neighbours' words"
  }
  public void test9_GotSadThinkingNeighbor() {
    doTranslateTest 'Мы опять приуныли, вдумываясь в слова соседа',
                    "We got sad again, thinking carefully about neighbour's words"
  }
  public void test9_12WePrefix() {
    doTranslateTest 'Мы приуныли, так как нам ее слова показались лишенными всякого смысла',
                    "We got sad because her words were void of any meaning"
  }

  public void test9_12JustMeaning() {
    doTranslateTest 'Мы приуныли, так как ее слова нам показались лишенными смысла',
                    "We got sad because her words were void of meaning"
  }

  public void test9_12Meaningless() {
    doTranslateTest 'Мы приуныли, так как ее слова нам показались бессмысленными',
                    "We got sad because her words were meaningless"
  }

  public void test9_2Only() {
    doTranslateTest "Ее слова нам показались лишенными всякого смысла",
            "Her words seemed void of any meaning to us"
  }
  public void test9_2WePrefix() {
    doTranslateTest "Нам ее слова показались лишенными всякого смысла",
            "To us, her words seemed void of any meaning"
  }

  public void test9_2JustMeaning() {
    doTranslateTest "Ее слова нам показались лишенными смысла",
            "Her words seemed void of meaning to us"
  }
  public void test9_2NoUs() {
    doTranslateTest "Ее слова показались лишенными смысла",
            "Her words seemed void of meaning"
  }

  public void test9_2Meaningless() {
    doTranslateTest "Ее слова нам показались бессмысленными",
            "Her words seemed meaningless to us"
  }
  public void test9_2Smart() {
    doTranslateTest "Ее речь нам кажется умной",
            "Her speech seems clever to us."
  }
  public void test9_SmartUsPrefix() {
    doTranslateTest "Нам ее речь кажется умной.",
            "To us, her speech seems clever."
  }

  public void test10_WhatToDo() {
    doTranslateTest 'Что делать?',
                    'What to do?'
  }
  public void test10_WhatToDo_Me() {
    doTranslateTest 'Что мне делать?',
                    'What am I supposed to do?'
  }
  public void test10_WhatToAsk() {
    doTranslateTest 'Что спросить?',
                    'What to ask?'
  }
  public void test10_WhatToAsk_Me() {
    doTranslateTest 'Что мне спросить?',
                    'What am I supposed to ask?'
  }
  public void test10_Me() {
    doTranslateTest 'Что мне было делать?',
                    'What was I supposed to do?'
  }
  public void test10_INeedPresent() {
    doTranslateTest 'Что мне нужно?',
                    'What do I need?'
  }
  public void test10_INeedPast() {
    doTranslateTest 'Что мне было нужно?',
                    'What did I need?'
  }
  public void test10_WhatDidWeDo() {
    doTranslateTest 'Что мы делали?',
                    'What did we do?'
  }
  public void test10_WhatDidIDo() {
    doTranslateTest 'Что я делал?',
                    'What did I do?'
  }
  public void test10_Subordinate() {
    doTranslateTest 'Я спросил, что делать.',
                    'I asked what to do.'
  }

  public void test11_JustGarden() {
    doTranslateTest 'Мы пошли в сад',
                    'We went to the garden'
  }
  public void test11_LittleGardenOnTheCorner() {
    doTranslateTest 'Мы пошли в маленький сад на углу',
                    'We went to the small garden on the corner'
  }
  public void test11_BigGardenOnTheCorner() {
    doTranslateTest 'Мы пошли в большой сад, что на углу Знаменской и Бассейной улиц',
                    "We went to the big garden, the one that's on the corner of Znamenskaya and Basseinaya streets"
  }
  public void test11_CountMouney() {
    doTranslateTest 'Мы стали считать деньги',
                    'We started counting money'
  }
  public void test11_WaterTrees() {
    doTranslateTest 'Мы стали поливать деревья',
                    'We started watering trees'
  }
  public void test11_Dance() {
    doTranslateTest 'Мы стали танцевать',
                    'We started dancing'
  }
  public void test12_StartedCounting() {
    doTranslateTest 'Мы начали считать деревья',
                    'We started counting trees'
  }
  public void test12_ReachingGardenStartedCounting() {
    doTranslateTest 'Дойдя до сада, мы начали считать деревья',
                    'Reaching the garden, we started counting trees'
  }
  public void test12_Reaching5() {
    doTranslateTest 'Дойдя до 5-ти, мы начали спорить',
                    'Reaching a five, we started arguing'
  }
  public void test12_ButReachingFive() {
    doTranslateTest 'Но, дойдя до 5, мы начали спорить',
                    'But reaching a 5, we started arguing'
  }
  public void test12_ReachingFivePostfix() {
    doTranslateTest 'Но мы начали спорить, дойдя до 5',
                    'But we started arguing, reaching a 5'
  }
  public void test12_ReachingSixInLetters() {
    doTranslateTest 'Дойдя до шести, мы начали спорить',
                    'Reaching a six, we started arguing'
  }
  public void test12_MyTheirOpinion() {
    doTranslateTest 'По моему мнению, раньше идет 7, а по их мнению - 8',
                    'In my opinion, a 7 comes first; but in their opinion an 8 does'
  }
  public void test12_HumbleOpinions() {
    doTranslateTest 'По моему скромному мнению, раньше идет 7, а по их скромному мнению - 8',
                    'In my humble opinion, a 7 comes first; but in their humble opinion an 8 does'
  }
  public void test12_ImhoVsCashier() {
    doTranslateTest 'По-моему, раньше идет 7, а по мнению кассирши - 8',
                    'In my opinion, a 7 comes first; but in the opinion of the cashier an 8 does'
  }
  public void test12_ImhoVsCashierFront7() {
    doTranslateTest 'По-моему, 7 идет раньше, а по мнению кассирши - 8',
                    'In my opinion, a 7 comes first; but in the opinion of the cashier an 8 does'
  }
  public void test12_ImhoVsCashierLater() {
    doTranslateTest 'По-моему, 7 идет раньше, а по мнению кассирши - потом',
                    'In my opinion, a 7 comes first; but in the opinion of the cashier, it comes next'
  }
  public void test12_AccordingCashier() {
    doTranslateTest 'По словам кассирши, раньше идет 7',
                    'According to the cashier, a 7 comes first'
  }
  public void test12_7Next() {
    doTranslateTest 'По словам кассирши, потом идет 7',
                    'According to the cashier, a 7 comes next'
  }
  public void test12_ShortNoEllipsis() {
    doTranslateTest 'По мнению одних дальше следовало 7, по мнению других дальше следовало 8',
                    'In the opinion of some, a 7 went next; but in opinion of others, an 8 went next'
  }
  public void test12_Before7Followed() {
    doTranslateTest 'Раньше потом следовало 7',
                    'Before, a 7 went next'
  }
  public void test12_7After8() {
    doTranslateTest 'По словам кассирши, после 8 идет 7',
                    'According to the cashier, a 7 comes after an 8'
  }
  public void test12_7After6() {
    doTranslateTest 'После 6-ти идет 7',
                    'A 7 comes after a six'
  }
  public void test12_7WentAfter6() {
    doTranslateTest 'После 6 следовало 7',
                    'A 7 went after a 6'
  }
  public void testCashierGoesToTheShop() {
    doTranslateTest 'Кассирша идет в магазин',
                    'The cashier goes to the store'
  }
  public void _testCashierFromBasseinayaGoesToTheShop() {
    doTranslateTest 'Кассирша с Бассейной идет в магазин',
                    'The cashier from Basseinaya street goes to the store'
  }
  public void _testCashierFromBasseinayaStreetGoesToTheShop() {
    doTranslateTest 'Кассирша с Бассейной улицы идет в магазин',
                    'The cashier from Basseinaya street goes to the store'
  }
  public void _testCashierFromStreetBasseinayaGoesToTheShop() {
    doTranslateTest 'Кассирша с улицы Бассейной идет в магазин',
                    'The cashier from the street Basseinaya goes to the store'
  }
  public void _testCashierGoesFromStreetBasseinayaToTheShop() {
    doTranslateTest 'Кассирша идет с улицы Бассейной в магазин',
                    'The cashier goes from the street Basseinaya to the store'
  }
  public void _testFemaleCashierGoesToTheShopMaleToGarden() {
    doTranslateTest 'Кассирша идет в магазин, а кассир - в сад',
                    'The female cashier goes to the store, and the male cashier - to the garden'
  }
  public void testAfterWorkCashierGoesToTheGarden() {
    doTranslateTest 'После работы кассирша идет в сад',
                    'After the work, the cashier goes to the garden'
  }
  public void testCashierGoesToTheGardenAfterWork() {
    doTranslateTest 'Кассир идет в сад после работы',
                    'The cashier goes to the garden after the work'
  }
  public void testAfterVasyaGoesCashier() {
    doTranslateTest 'После Васи идет кассир',
                    'After Vasya, the cashier goes'
  }
  public void testCashierGoesAfterVasya() {
    doTranslateTest 'Кассир идет после Васи',
                    'The cashier goes after Vasya'
  }
  public void test12_7Before8() {
    doTranslateTest '7 идет раньше 8',
                    'A 7 comes before an 8'
  }
  public void test12_InOpinionCoordination() {
    doTranslateTest 'По мнению кассирши и по мнению других, семь идет раньше восьми',
                    'In the opinion of the cashier and in opinion of others, a seven comes before an eight'
  }
  public void test12_InOpinionCoordinationNumbers() {
    doTranslateTest 'По мнению кассирши и по мнению других, 7 идет раньше 8',
                    'In the opinion of the cashier and in opinion of others, a 7 comes before an 8'
  }
  public void test12_OpinionCoordination() {
    doTranslateTest 'По мнению кассирши и мнению других, семь идет раньше восьми',
                    'In the opinion of the cashier and opinion of others, a seven comes before an eight'
  }
  public void test12_OpinionCoordinationNumbers() {
    doTranslateTest 'По мнению кассирши и мнению других, 7 идет раньше 8',
                    'In the opinion of the cashier and opinion of others, a 7 comes before an 8'
  }
  public void test12_OpinionantCoordination() {
    doTranslateTest 'По мнению кассирши и других, 7 идет раньше 8',
                    'In the opinion of the cashier and others, a 7 comes before an 8'
  }
  public void test12_StoreCashier() {
    doTranslateTest 'По мнению кассирши магазина, семь идет раньше восьми',
                    'In the opinion of the store cashier, a seven comes before an eight'
  }
  public void test12_StoreCashierAndOthers() {
    doTranslateTest 'По мнению кассирши магазина и других, 7 идет раньше 8',
                    'In the opinion of the store cashier and others, a 7 comes before an 8'
  }
  public void test12_OpinionCoordinationWithWords_SamePerson() {
    doTranslateTest 'По словам и мнению кассирши, семь идет раньше восьми',
                    'According to and in the opinion of the cashier, a seven comes before an eight'
  }
  public void test12_OpinionCoordinationWithWords_SamePersonNumbers() {
    doTranslateTest 'По словам и мнению кассирши, 7 идет раньше 8',
                    'According to and in the opinion of the cashier, a 7 comes before an 8'
  }
  public void test12_OpinionCoordinationWithWords() {
    doTranslateTest 'По словам кассирши и по мнению других, 7 идет раньше 8',
                    'According to the cashier and in opinion of others, a 7 comes before an 8'
  }
  public void test12_OpinionCoordinationWithWords_SamePerson_RepeatedPreposition() {
    doTranslateTest 'По словам и по мнению кассирши, 7 идет раньше 8',
                    'According to and in the opinion of the cashier, a 7 comes before an 8'
  }

  public void test13_LuckMissingNoCommas() {
    doTranslateTest 'Мы спорили бы очень долго, но по счастию тут со скамейки свалился какой-то ребенок и сломал себе обе челюсти',
            'We were arguing for a long time, when by some sheer luck, a child fell off a bench and broke both of his jaws'
  }
  public void test13_LuckMissingComma1() {
    doTranslateTest 'Мы спорили бы очень долго, но по счастию, тут со скамейки свалился какой-то ребенок и сломал себе обе челюсти',
            'We were arguing for a long time, when by some sheer luck, a child fell off a bench and broke both of his jaws'
  }
  public void test13_InnerClause() {
    doTranslateTest 'По счастию, тут со скамейки свалился какой-то ребенок',
            'By some sheer luck, a child fell off a bench'
  }
  public void test13_InnerClauseLuckMissingComma2() {
    doTranslateTest 'По счастию тут со скамейки свалился какой-то ребенок',
            'By some sheer luck, a child fell off a bench'
  }
  public void _test13_LuckMissingComma2() {
    doTranslateTest 'Мы спорили бы очень долго, но, по счастию тут со скамейки свалился какой-то ребенок',
            'We were arguing for a long time, when by some sheer luck, a child fell off a bench'
  }

  public void test13_OneJaw() {
    doTranslateTest 'Ребенок свалился и сломал себе челюсть',
                    'The child fell and broke his jaw'
  }
  public void test13_ManyJaws() {
    doTranslateTest 'Ребенок свалился и сломал себе челюсти',
                    'The child fell and broke his jaws'
  }
  public void test13_EightFingers() {
    doTranslateTest 'Ребенок сломал себе восемь пальцев',
                    'The child broke eight of his fingers'
  }
  public void test13_8Fingers() {
    doTranslateTest 'Ребенок сломал себе 8 пальцев',
                    'The child broke 8 of his fingers'
  }
  public void test13_BothJawsAndEightFingers() {
    doTranslateTest 'Ребенок сломал себе обе челюсти и восемь пальцев',
                    'The child broke both of his jaws and eight fingers'
  }
  public void test13_BothJawsAnd8Fingers() {
    doTranslateTest 'Ребенок сломал себе обе челюсти и 8 пальцев',
                    'The child broke both of his jaws and 8 fingers'
  }
  public void test13_BothJawsAndTwoFingers() {
    doTranslateTest 'Ребенок сломал себе обе челюсти и два пальца',
                    'The child broke both of his jaws and two fingers'
  }
  public void test13_ThreeJaws() {
    doTranslateTest 'Ребенок сломал себе 3 челюсти',
                    'The child broke 3 of his jaws'
  }
  public void test13_2Fingers() {
    doTranslateTest 'Ребенок сломал себе 2 пальца',
                    'The child broke 2 of his fingers'
  }
  public void test13_ThreeJawsAnd2Fingers() {
    doTranslateTest 'Ребенок сломал себе 3 челюсти и 2 пальца',
                    'The child broke 3 of his jaws and 2 fingers'
  }
  public void test13_2FingersAndAJaw() {
    doTranslateTest 'Ребенок сломал себе 2 пальца и челюсть',
                    'The child broke 2 of his fingers and a jaw'
  }
  public void test13_BothJawsAndOneFinger() {
    doTranslateTest 'Ребенок сломал себе обе челюсти и один палец',
                    'The child broke both of his jaws and one finger'
  }
  public void test13_BothJawsAndFinger() {
    doTranslateTest 'Ребенок сломал себе обе челюсти и палец',
                    'The child broke both of his jaws and a finger'
  }
  public void test13_MeFingers() {
    doTranslateTest 'Ребенок сломал мне три пальца',
                    'The child broke three of my fingers'
  }
  public void test13_FemaleCashierFingers() {
    doTranslateTest 'Кассирша сломала себе 8 пальцев',
                    'The cashier broke 8 of her fingers'
  }
  public void test13_MaleCashierFingers() {
    doTranslateTest 'Кассир сломал себе 2 пальца',
                    'The cashier broke 2 of his fingers'
  }
  public void test14_Argument() {
    doTranslateTest 'Это отвлекло нас от спора',
            'That distracted us from the argument'
  }
  public void test14_TheirArgument() {
    doTranslateTest 'Это отвлекло их от их спора',
            'That distracted them from their argument'
  }

  public void test15_They() {
    doTranslateTest 'А потом они разошлись по домам.',
            'And then they all went home.'
  }

  public void test15_ThenThey() {
    doTranslateTest 'Потом они разошлись по домам.',
            'Then they all went home.'
  }
  public void test15_AndThey() {
    doTranslateTest 'А они разошлись по домам.',
            'And they all went home.'
  }
  public void test15_Rooms() {
    doTranslateTest 'А они разошлись по своим комнатам.',
            'And they all went to their rooms.'
  }
  public void test15_RoomsApartments() {
    doTranslateTest 'А они разошлись по своим комнатам и квартирам',
            'And they all went to their rooms and apartments'
  }
  public void test15_RoomsApartmentsOffices() {
    doTranslateTest 'А они разошлись по своим комнатам, квартирам и офисам.',
            'And they all went to their rooms, apartments and offices.'
  }
  public void test15_TheirOffices() {
    doTranslateTest 'Мы разошлись по их офисам.',
            'We all went to their offices.'
  }

}
