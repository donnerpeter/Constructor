package constructor

import constructor.russian.RussianLexicon

/**
 * @author peter
 */

class SyntaxTest extends GroovyTestCase {

  void testRechnik1() {
    _ '''Власти московской управы "Крылатское" намерены в течение месяца решить вопрос о сносе
незаконных строений в поселке "Речник"''', """
NounObj #1=власти #2=управы
Subject #3=намерены #1
AdjNoun московской #2
Appos
  Quoted
    "
    Color{1}
      крылатское
    "
  #2
XComp #3 #4=решить
When
  Prepos 'в течение' месяца
  #4
'Решить вопрос' #4 вопрос о #5=сносе
NounObj #5 #6=строений
AdjNoun незаконных #6
Where
  Prepos в #7=поселке
  #6
Appos
  Quoted
    "
    Color{2}
      речник
    "
  #7
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
      Subject #2=планируем мы
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
Subject #1 #5=глава
NounObj #5 #6=управы
Named
  NameSurname виталий никитин
  #5
Appos
  Quoted
    "
    Color{2}
      крылатское
    "
  #6  
"""
  }

  public void testRechnik3() {
    _ 'Он сообщил, что с утра уже демонтированы два строения, к сносу готовится третий дом', """
Subject #1=сообщил он
Comp #1 , что #2=демонтированы
When
  Prepos с утра
  #2
Already уже #2
Subject
  #2
  Quantity два строения
,
Oblique #3=готовится к сносу
Subject #3 #4=дом
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
    Color{2}
      When
        XWhen
          когда
          #2='дать добро' дают #3=добро
        ,
        #4=сносим
      Subject #2 приставы
      Goal #2 нам
      Quoted
        "
        Color{1}
          #3
        "
      Subject #4 мы
      Demonstrative эти #5=дома
      Obj #4 #5
      .
      Subject #6=снесли мы
      NegObj не #6 ни одного #7=дома
      Relative #8=котором #7 ,
      Where
        Prepos в #8
        #9=проживали
      Subjunctive бы #9
      Subject #9 #10=жители
      AdjNoun местные #10
    "
  ,
  -
Subject #1 чиновник
.
"""

  }

  public void testRechnik5() throws Exception {
    _ 'По его словам, всего в поселке "Речник" планируется к сносу 42 постройки, из которых по 37 уже есть судебные решения.', """
Comma
  #1='По словам' по его словам
  ,
Source #1 #2=планируется
'Всего+Утверждение о количестве'
  всего
  #2
  #3=Quantity 42 постройки
Where
  Prepos в #4=поселке
  #2
Appos
  Quoted
    "
    Color{1}
      речник
    "
  #4
Oblique #2 к сносу
Subject #2 #3
Relative #5=которых #3 ,
Subset #6=37 из #5
Copula
  #7=решения
  #8=есть
  Prepos #9=по #6
About #7 #9 #6
Already уже #8
AdjNoun судебные #7
.
"""
  }

  def _(input, output) {
    assertEquals output.trim(), new Parser(new RussianLexicon()).parse(input).prettyPrint().trim()
  }

}