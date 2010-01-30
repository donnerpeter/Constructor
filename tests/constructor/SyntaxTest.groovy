package constructor

import constructor.russian.RussianLexicon

/**
 * @author peter
 */

class SyntaxTest extends GroovyTestCase {

  void testRechnik1() {
    //Власти московской управы "Крылатское" намерены в течение месяца решить вопрос о сносе незаконных строений в
    //поселке "Речник".

    _ "Власти московской управы", """
Posess Власти #1=управы
AdjNoun московской #1
"""

  }

  def _(input, output) {
    assertEquals output.trim(), new Parser(new RussianLexicon()).parse(input).trim()
  }

}