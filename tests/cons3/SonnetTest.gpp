package cons3

import junit.framework.TestCase

/**
 * @author peter
 */
class SonnetTest extends TestCase {

  public void testParse1() {
    doParseTest("Удивительный случай случился со мной: я вдруг забыл, что идет раньше - 7 или 8", """
A.property=AMAZING
A.type=THING
B.experiencer=C
B.arg1=A
B.type=HAPPEN
B.time=PAST
C.type=ME
-- 2:
A.type=ME
B.type=FORGET
B.time=PAST
B.arg1=A
A.gender=masc
A.person=3
B.manner=SUDDENLY
B@1.elaboration=B
-- 3:
A.type=wh
B@2.arg2=B
C.time=PRESENT
C.arg1=A
A.person=3
B.content=C
B.type=question
B.questioned=A
C.type=COME_SCALARLY
C.order=EARLIER
C.anchor=D
-- 4:
A@3.variants=A
B.type=7
B.number=true
A.member=B
C.type=8
C.number=true
A.conj=or
A.member=C
""")
  }

  static void doParseTest(String input, String expected) {
    Variable.counter = 0
    Tokens.counter = 0
    def parser = new Parser()
    def chart = parser.parse(input)
    def result = chart.presentable()
    if (expected.trim() != result) {
      parser.printLog()
      assertEquals expected.trim() + "\n", result + "\n"
    }
  }

  public void testParse2() {
    doParseTest "Я отправился к соседям и спросил их, что они думают по этому поводу", """
A.type=ME
B.type=GO_OFF
B.time=PAST
B.arg1=A
B.goal=C
C.type=NEIGHBOURS
D.type=ASK
D.time=PAST
D.arg2=E
D.arg1=A
D.topic=F
G.member=B
G.conj=and
G.member=D
E.type=THEY
-- 2:
A.type=wh
D@1.question=B
C.type=THEY
D.type=THINK
D.time=PRESENT
D.topic=E
D.arg1=C
D.arg2=A
B.content=D
B.type=question
B.questioned=A
E.determiner=THIS
E.type=MATTER
"""
  }

  public void testParse3() {
    doParseTest "Каково же было их и мое удивление, когда они вдруг обнаружили, что тоже не могут вспомнить порядок счета.", """
A.type=wh
B.type=degree
B.arg1=C
B.arg2=A
B.time=PAST
D.type=THEY
E.type=ME
F.member=D
F.conj=and
F.member=E
C.type=AMAZE
C.arg1=F
-- 2:
A.type=THEY
B.type=DISCOVER
B.time=PAST
B.arg1=A
B.manner=SUDDENLY
B@1.whenCondition=B
-- 3:
B@2.theme=A
B.type=CAN
B.time=PRESENT
B.arg1=C
B.negated=true
B.also=true
A.type=fact
A.content=B
D.type=RECALL
D.arg2=E
D.arg1=F
B.theme=D
E.type=ORDER
E.criterion=G
G.type=COUNTING
B.dot=true
"""
  }

  public void testParse3_4() {
    doParseTest "Каково же было их и мое удивление, когда они вдруг обнаружили, что тоже не могут вспомнить порядок счета. " +
                "1, 2, 3, 4, 5 и 6 помнят, а дальше забыли.", """
A.type=wh
B.type=degree
B.arg1=C
B.arg2=A
B.time=PAST
D.type=THEY
E.type=ME
F.member=D
F.conj=and
F.member=E
C.type=AMAZE
C.arg1=F
-- 2:
A.type=THEY
B.type=DISCOVER
B.time=PAST
B.arg1=A
B.manner=SUDDENLY
B@1.whenCondition=B
-- 3:
B@2.theme=A
B.type=CAN
B.time=PRESENT
B.arg1=C
B.negated=true
B.also=true
A.type=fact
A.content=B
D.type=RECALL
D.arg2=E
D.arg1=F
B.theme=D
E.type=ORDER
E.criterion=G
G.type=COUNTING
B.dot=true
-- 4:
A.type=1
A.number=true
-- 5:
A.type=2
A.number=true
B.member=A@4
B.member=A
-- 6:
A.type=3
A.number=true
B@5.member=A
-- 7:
A.type=4
A.number=true
B@5.member=A
-- 8:
A.type=5
A.number=true
B@5.member=A
B.type=6
B.number=true
B@5.conj=and
B@5.member=B
C.type=REMEMBER
C.time=PRESENT
C.arg1=D
D.rusNumber=pl
D.person=3
C.arg2=B@5
-- 9:
A.andEmphasis=true
B.type=NEXT
A.type=FORGET
A.time=PAST
A.arg1=D@8
D@8.gender=pl
A.arg2=B
C.member=C@8
C.conj=but
C.member=A
A.dot=true
"""
  }

  public void testParse5() {
    doParseTest 'Мы все пошли в коммерческий магазин "Гастроном", что на углу Знаменской и Бассейной улицы, и спросили кассиршу о нашем недоумении', """
A.type=WE
A.quantifier=ALL
B.type=GO
B.time=PAST
B.arg1=A
B.goal=C
C.kind=COMMERCIAL
C.type=SHOP
C.name=гастроном
-- 2:
A.type=wh
B.location=C
B.type=copula
B.arg1=A
D.content=B
D.type=question
D.questioned=A
C.type=CORNER
C.arg1=E
F.type=STREET
F.name=знаменская
G.type=STREET
G.name=бассейная
E.member=F
E.conj=and
E.member=G
C@1.relative=D
-- 3:
A.type=ASK
A.time=PAST
A.arg2=B
A.arg1=A@1
A.topic=C
D.member=B@1
D.conj=and
D.member=A
B.type=CASHIER
E.type=WE
C.type=PREDICAMENT
C.arg1=E
"""
  }

  public void testParse6() {
    doParseTest "Кассирша грустно улыбнулась, вынула изо рта маленький молоточек и, слегка подвигав носом, сказала:", """
A.type=CASHIER
A.gender=fem
B.type=SMILE
B.time=PAST
B.arg1=A
B.manner=SADLY
-- 2:
A.type=TAKE_OUT
A.time=PAST
A.arg1=A@1
A.arg2=B
A.source=C
D.member=B@1
D.member=A
C.type=MOUTH
B.size=LITTLE
B.type=HAMMER
-- 3:
A.type=MOVE
A.arg2=B
A.manner=SLIGHTLY
B.type=NOSE
C.perfectBackground=A
-- 4:
C@3.type=SAY
C@3.time=PAST
C@3.arg1=A@1
C@3.addressee=A
D@2.conj=and
D@2.member=C@3
C@3.message=B
B.directSpeech=true
"""
  }

  public void testParse7() {
    doParseTest "По-моему, семь идет после восьми в том случае, когда восемь идет после семи", """
A.type=OPINION
A.arg1=B
B.type=ME
C.opinion_of=A
-- 2:
A.type=7
C@1.time=PRESENT
C@1.condition=B
C@1.arg1=A
A.person=3
C@1.type=COME_SCALARLY
C@1.order=AFTER
C@1.anchor=C
C.type=8
B.determiner=THAT
B.type=CASE
-- 3:
B@2.whenCondition=A
B.type=8
A.time=PRESENT
A.arg1=B
B.person=3
A.type=COME_SCALARLY
A.order=AFTER
A.anchor=C
C.type=7
"""
  }

  public void testParse6_7() {
    doParseTest """Кассирша грустно улыбнулась, вынула изо рта маленький молоточек и, слегка подвигав носом, сказала:
- По-моему, семь идет после восьми в том случае, когда восемь идет после семи""", """
A.type=CASHIER
A.gender=fem
B.type=SMILE
B.time=PAST
B.arg1=A
B.manner=SADLY
-- 2:
A.type=TAKE_OUT
A.time=PAST
A.arg1=A@1
A.arg2=B
A.source=C
D.member=B@1
D.member=A
C.type=MOUTH
B.size=LITTLE
B.type=HAMMER
-- 3:
A.type=MOVE
A.arg2=B
A.manner=SLIGHTLY
B.type=NOSE
C.perfectBackground=A
-- 4:
C@3.type=SAY
C@3.time=PAST
C@3.arg1=A@1
C@3.addressee=A
D@2.conj=and
D@2.member=C@3
-- 6:
C@3.message=A
A.directSpeech=true
B.type=OPINION
B.arg1=C
C.type=ME
A.opinion_of=B
-- 7:
A.type=7
A@6.time=PRESENT
A@6.condition=B
A@6.arg1=A
A.person=3
A@6.type=COME_SCALARLY
A@6.order=AFTER
A@6.anchor=C
C.type=8
B.determiner=THAT
B.type=CASE
-- 8:
B@7.whenCondition=A
B.type=8
A.time=PRESENT
A.arg1=B
B.person=3
A.type=COME_SCALARLY
A.order=AFTER
A.anchor=C
C.type=7
"""
  }

  public void testParse8() {
    doParseTest """Мы поблагодарили кассиршу и с радостью выбежали из магазина""", """
A.type=WE
B.type=THANK
B.time=PAST
B.arg2=C
B.arg1=A
C.type=CASHIER
D.type=JOY
E.type=RUN_OUT
E.time=PAST
E.source=F
E.arg1=A
E.mood=D
G.member=B
G.conj=and
G.member=E
F.type=SHOP
"""
  }


  public void testTranslate1() {
    doTranslateTest "Удивительный случай случился со мной: я вдруг забыл, что идет раньше - 7 или 8",
                    "An amazing thing happened to me today, I suddenly forgot what comes first - 7 or 8"
  }

  public void testTranslate2() {
    doTranslateTest "Я отправился к соседям и спросил их, что они думают по этому поводу",
                    "I went to my neighbors and asked them about their opinion on this matter"
  }

  public void testTranslate3() {
    doTranslateTest "Каково же было их и мое удивление, когда они вдруг обнаружили, что тоже не могут вспомнить порядок счета.",
                    "Great was their and my amazement, when they suddenly discovered, that they couldn't recall the counting order."
  }

  public void testTranslate4() {
    doTranslateTest "1, 2, 3, 4, 5 и 6 помнят, а дальше забыли.",
                    "They remember 1, 2, 3, 4, 5 and 6, but forgot what comes next."
  }

  public void testTranslate3_4() {
    doTranslateTest "Каково же было их и мое удивление, когда они вдруг обнаружили, что тоже не могут вспомнить порядок счета. " +
                            "1, 2, 3, 4, 5 и 6 помнят, а дальше забыли.",
                    "Great was their and my amazement, when they suddenly discovered, that they couldn't recall the counting order. " +
                            "They remembered 1, 2, 3, 4, 5 and 6, but forgot what comes next."
  }

  public void testTranslate5() {
    doTranslateTest 'Мы все пошли в коммерческий магазин "Гастроном", что на углу Знаменской и Бассейной улицы, и спросили кассиршу о нашем недоумении',
                    "We all went to a commercial grocery store, the one that's on the corner of Znamenskaya and Basseinaya streets to consult a cashier on our predicament"
  }

  public void testTranslate6() {
    doTranslateTest 'Кассирша грустно улыбнулась, вынула изо рта маленький молоточек и, слегка подвигав носом, сказала:',
                    "The cashier gave us a sad smile, took a small hammer out of her mouth, and moving her nose slightly back and forth, she said:"
  }

  public void testTranslate7() {
    doTranslateTest 'По-моему, семь идет после восьми в том случае, когда восемь идет после семи',
                    "In my opinion, a seven comes after an eight, only if an eight comes after a seven"
  }

  public void testTranslate6_7() {
    doTranslateTest '''Кассирша грустно улыбнулась, вынула изо рта маленький молоточек и, слегка подвигав носом, сказала:
- По-моему, семь идет после восьми в том случае, когда восемь идет после семи''',
                    """The cashier gave us a sad smile, took a small hammer out of her mouth, and moving her nose slightly back and forth, she said:
- In my opinion, a seven comes after an eight, only if an eight comes after a seven"""
  }

  public void testTranslate8() {
    doTranslateTest 'Мы поблагодарили кассиршу и с радостью выбежали из магазина',
                    "We thanked the cashier and ran cheerfully out of the store"
  }

  public void testParse9() {
    doParseTest 'Но тут, вдумываясь в слова кассирши, мы опять приуныли, так как ее слова показались нам лишенными всякого смысла.', '''A.butEmphasis=true
A.emphasis=true
-- 2:
A.type=THINK
A.theme=B
B.type=WORDS
B.author=C
C.type=CASHIER
A@1.perfectBackground=A
-- 3:
A.type=WE
A@1.type=GET_SAD
A@1.time=PAST
A@1.arg1=A
A@1.anchor=AGAIN
-- 4:
A@1.reason=A
B.type=SHE
C.type=WORDS
C.author=B
A.type=SEEM
A.time=PAST
A.arg1=C
D.type=WE
E.type=LACK
E.arg2=F
A.theme=E
F.determiner=ANY
F.type=MEANING
A.dot=true
'''
  }

  public void testTranslate9() {
    doTranslateTest 'Но тут, вдумываясь в слова кассирши, мы опять приуныли, так как ее слова показались нам лишенными всякого смысла.',
            "But there, thinking carefully about cashier's words, we got sad again because her words were void of any meaning."
  }

  public void testParse10() {
    doParseTest 'Что нам было делать?', '''
A.type=wh
B.type=WE
C.type=DO
C.arg2=A
C.arg1=B
D.time=PAST
D.type=modality
D.arg1=C
E.content=D
E.type=question
E.questioned=A
'''
  }

  public void testTranslate10() {
    doTranslateTest 'Что нам было делать?', 'What were we supposed to do?'
  }

  public void testParse11() {
    doParseTest 'Мы пошли в Летний сад и стали там считать деревья', '''
A.type=WE
B.type=GO
B.time=PAST
B.arg1=A
B.goal=C
C.type=GARDEN
C.name=Летний сад
D.type=BEGIN
D.time=PAST
D.arg1=A
E.member=B
E.conj=and
E.member=D
F.type=COUNT
F.arg2=G
F.arg1=H
D.theme=F
G.type=TREES
'''
  }

  public void testTranslate11() {
    doTranslateTest 'Мы пошли в Летний сад и стали там считать деревья',
                    'We went to the Summer Garden and started counting trees'
  }

  public void testParse12() {
    doParseTest 'Но дойдя в счете до 6-ти, мы остановились и начали спорить: по мнению одних дальше следовало 7, по мнению других - 8', '''
A.butEmphasis=true
B.type=COME_TO
B.domain=C
B.goal=D
C.type=COUNTING
D.type=6
A.perfectBackground=B
-- 2:
A.type=WE
A@1.type=STOP
A@1.time=PAST
A@1.arg1=A
B.type=BEGIN
B.time=PAST
B.arg1=A
C.member=A@1
C.conj=and
C.member=B
D.type=ARGUE
D.arg1=E
B.theme=D
-- 3:
A.type=OPINION
A.arg1=B
C.opinion_of=A
B.type=SOME
C.time=PAST
C.arg1=D
C.type=COME_SCALARLY
C.order=AFTER
D.type=7
D.number=true
-- 4:
A.type=OPINION
A.arg1=B
C.opinion_of=A
B.type=OTHERS
-- 5:
A.type=ellipsis
<A>
C@4.time=PAST
C@4.arg1=B
C@4.type=COME_SCALARLY
C@4.order=AFTER
D@2.elaboration=C
C.member=C@3
C.member=C@4
</A>
B.type=8
B.number=true
'''
  }

  public void testTranslate12() {
    doTranslateTest 'Но дойдя в счете до 6-ти, мы остановились и начали спорить: по мнению одних дальше следовало 7, по мнению других - 8',
                    'But reaching a six in count, we stopped and started arguing: in the opinion of some, a 7 went next; but in opinion of others an 8 did'
  }

  public void testParse13() {
    doParseTest "Мы спорили бы очень долго, но, по счастию, тут со скамейки свалился какой-то ребенок и сломал себе обе челюсти", """
A.type=WE
B.type=ARGUE
B.time=PAST
B.arg1=A
B.irrealis=true
B.duration=LONG
-- 2:
A.butEmphasis=true
-- 3:
A.type=LUCK
A.topic=A@2
-- 4:
A@2.emphasis=true
A.type=BENCH
A@2.type=FALL
A@2.time=PAST
A@2.arg1=B
A@2.source=A
B.determiner=SOME
B.type=CHILD
C.type=BREAK
C.time=PAST
C.arg1=B
C.arg2=D
B.gender=masc
E.member=B@1
E.conj=but
E.member=F
F.member=A@2
F.conj=and
F.member=C
G.type=BOTH
D.arg1=B
D.type=JAWS
D.quantifier=G
"""
  }

  public void testTranslate13() {
    doTranslateTest 'Мы спорили бы очень долго, но, по счастию, тут со скамейки свалился какой-то ребенок и сломал себе обе челюсти',
                    'We were arguing for a long time, when by some sheer luck, a child fell off a bench and broke both of his jaws'
  }

  public void testParse14() {
    doParseTest 'Это отвлекло нас от нашего спора', '''
A.type=THAT
B.type=DISTRACT
B.time=PAST
B.theme=C
B.arg1=A
B.arg2=D
D.type=WE
E.type=WE
C.type=ARGUE
C.arg1=E
'''
  }

  public void testTranslate14() {
    doTranslateTest 'Это отвлекло нас от нашего спора',
            'That distracted us from our argument'
  }

  public void testParse15() {
    doParseTest 'А потом мы разошлись по домам', '''
A.andEmphasis=true
B.type=WE
A.type=DISPERSE
A.time=PAST
A.arg1=B
A.goal=C
A.relTime=AFTER
C.type=HOMES
C.owner=D
'''
  }

  public void testTranslate15() {
    doTranslateTest 'А потом мы разошлись по домам.',
            'And then we all went home.'
  }

  static void doTranslateTest(String input, String expected) {
    Variable.counter = 0
    Tokens.counter = 0
    def parser = new Parser()

    Chart chart
    try {
      chart = parser.parse(input)
    } catch (Throwable e) {
      parser.printLog()
      throw e
    }

    String actual
    try {
      actual = new EnglishGenerator().generate(chart)
    } catch (Throwable e) {
      println "\nChart:\n\n${chart.presentable()}"
      parser.printLog()
      throw e
    }

    if (actual != expected) {
      println "\nChart:\n\n${chart.presentable()}"
      parser.printLog()
    }

    assertEquals expected, actual
  }
}
