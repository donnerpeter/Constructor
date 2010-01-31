package constructor

import constructor.russian.RussianLexicon

/**
 * @author peter
 */

class SyntaxTest extends GroovyTestCase {

  void testRechnik1() {
    //Власти московской управы "Крылатское" намерены в течение месяца решить вопрос о сносе незаконных строений в
    //поселке "Речник".

    _ 'Власти московской управы "Крылатское" намерены', """
NounGenitive #1=Власти #2=управы
SubjPred #1 намерены
AdjNoun московской #2
Appos
  #2
  Quoted " Крылатское "
"""

  }

  def _(input, output) {
    assertEquals output.trim(), new Parser(new RussianLexicon()).parse(input).trim()
  }

}