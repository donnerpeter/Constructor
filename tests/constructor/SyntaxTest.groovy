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
      Речник
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
    Color{2}
      When
        XWhen Когда #2=дают
        ,
        #3=сносим
      SubjPred #2 приставы
      Goal #2 нам
      'дать добро' #2 #4=добро
      Quoted
        "
        Color{1}
          #4
        "
      SubjPred #3 мы
      Demonstrative эти #5=дома
      Obj #3 #5
      .
      SubjPred #6=снесли Мы
      NegObj не #6 ни одного #7=дома
      Relative #8=котором #7 ,
      Where
        Prepos в #8
        #9=проживали
      Subjunctive бы #9
      SubjPred #9 #10=жители
      AdjNoun местные #10
    "
  ,
  -
SubjPred #1 чиновник
.
"""

  }

  public void testRechnik5() throws Exception {
    _ 'По его словам, всего в поселке "Речник" планируется к сносу 42 постройки, из которых по 37 уже есть судебные решения.', """

Source
  'По словам' По его словам ,
  #1=планируется
'Всего+Утверждение о количестве'
  всего
  #1
  #2=Quantity 42 постройки
Where
  Prepos в #3=поселке
  #1
Appos
  Quoted
    "
    Color{1}
      Речник
    "
  #3
Oblique #1 к сносу
SubjPred #1 #2
Relative #4=которых #2 ,
Subset #5=37 из #4
Copula
  #6=решения
  #7=есть
  Prepos #8=по #5
About #6 #8 #5
Already уже #7
AdjNoun судебные #6
.
"""
  }

  def _(input, output) {
    assertEquals output.trim(), new Parser(new RussianLexicon()).parse(input).prettyPrint().trim()
  }

}