package constructor

import constructor.russian.RussianLexicon

/**
 * @author peter
 */

class SyntaxTest extends GroovyTestCase {

  void testRechnik1() {
    _ '''Власти московской управы "Крылатское" намерены в течение месяца решить вопрос о сносе
незаконных строений в поселке "Речник"''', """
NounObj #1=Власти #2=управы
SubjPred #3=намерены #1
AdjNoun московской #2
Appos
  Quoted
    "
    Color{1}
      Крылатское
    "
  #2
XComp #3 #4=решить
When
  Prepos 'в течение' месяца
  #4
Obj #4 #5=вопрос
About
  Prepos о #6=сносе
  #5
NounObj #6 #7=строений
AdjNoun незаконных #7
Where
  Prepos в #8=поселке
  #7
Appos
  Quoted
    "
    Color{2}
      Речник
    "
  #8
"""
  }
  
  void testRechnik2() {
    _ '''"Мы планируем все незаконные постройки снести за месяц", - сообщил журналистам во вторник в поселке
глава управы "Крылатское" Виталий Никитин''', """
DirectSpeech
  #1=сообщил
  Quoted
    "
    Color{1}
      SubjPred #2=планируем Мы
      XComp #2 #3=снести
      Quantifier все #4=постройки
      AdjNoun незаконные #4
      Obj #3 #4
      When
        Prepos за месяц
        #3
    "
  ,
  -
Goal #1 журналистам
When
  Prepos во вторник
  #1
Where
  Prepos в поселке
  #1
SubjPred #1 #5=глава
NounObj #5 #6=управы
Named
  NameSurname Виталий Никитин
  #5
Appos
  Quoted
    "
    Color{2}
      Крылатское
    "
  #6  
"""
  }

  public void testRechnik3() {
    _ 'Он сообщил, что с утра уже демонтированы два строения, к сносу готовится третий дом', """
SubjPred #1=сообщил Он
Comp #1 , что #2=демонтированы
When
  Prepos с утра
  #2
Already уже #2
SubjPred
  #2
  Quantity два строения
,
Oblique #3=готовится к сносу
SubjPred #3 #4=дом
Order третий #4
"""
  }

  public void testRechnik4() {
    _ '''"Когда приставы нам дают "добро",
мы эти дома сносим. Мы не снесли ни одного дома, в котором бы проживали местные жители", - сказал чиновник.
''', """
DirectSpeech
  #1=сказал
  Quoted
    "
    Color{1}
      When
        XWhen Когда #2='дают "добро"'
        ,
        #3=сносим
      SubjPred #2 приставы
      Goal #2 нам
      SubjPred #3 мы
      Demonstrative эти #4=дома
      Obj #3 #4
      .
      SubjPred #5=снесли Мы
      NegObj не #5 ни одного #6=дома
      Relative #7=котором #6 ,
      Where
        Prepos в #7
        #8=проживали
      Subjunctive бы #8
      SubjPred #8 #9=жители
      AdjNoun местные #9
    "
  ,
  -
SubjPred #1 чиновник
.
"""

  }

  def _(input, output) {
    assertEquals output.trim(), new Parser(new RussianLexicon()).parse(input).trim()
  }

}