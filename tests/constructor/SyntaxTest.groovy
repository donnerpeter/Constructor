package constructor

import constructor.russian.RussianLexicon

/**
 * @author peter
 */

class SyntaxTest extends GroovyTestCase {

  void testRechnik1() {
    //Власти московской управы "Крылатское" намерены в течение месяца решить вопрос о сносе незаконных строений в
    //поселке "Речник".

    _ 'Власти московской управы "Крылатское" намерены в течение месяца решить вопрос о сносе', """
NounGenitive #1=Власти #2=управы
SubjPred #1 #3=намерены
AdjNoun московской #2
Appos
  #2
  Quoted " Крылатское "
XComp #3 #4=решить
When
  Prepos 'в течение' месяца
  #4
Obj #4 #5=вопрос
About
  Prepos о сносе
  #5 
"""

  }

  def _(input, output) {
    assertEquals output.trim(), new Parser(new RussianLexicon()).parse(input).trim()
  }

}