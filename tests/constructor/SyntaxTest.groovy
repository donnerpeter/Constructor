package constructor

import constructor.russian.RussianLexicon

/**
 * @author peter
 */

class SyntaxTest extends GroovyTestCase {

  void testRechnik1() {
    _ '''Власти московской управы "Крылатское" намерены в течение месяца решить вопрос о сносе
незаконных строений в поселке "Речник"''', """
NounGenitive #1=Власти #2=управы
SubjPred #1 #3=намерены
AdjNoun московской #2
Appos
  #2
  Quoted
    "
    Color{1}
      Крылатское
    "
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
  #8
  Quoted
    "
    Color{2}
      Речник
    "
"""
  }
  
  void testRechnik2() {
    //"Мы планируем все незаконные постройки снести за месяц", - сообщил журналистам во вторник в поселке глава управы
    //"Крылатское" Виталий Никитин.

    _ '''Мы планируем все незаконные постройки снести за месяц''', """
SubjPred Мы #1=планируем
XComp #1 #2=снести
Quantifier все #3=постройки
AdjNoun незаконные #3
Obj #2 #3
When
  Prepos за месяц
  #2
"""
  }

  def _(input, output) {
    assertEquals output.trim(), new Parser(new RussianLexicon()).parse(input).trim()
  }

}