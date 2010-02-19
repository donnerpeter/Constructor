package constructor

import constructor.russian.RussianLexicon

 /**
 * @author peter
 */

class FramesTest extends GroovyTestCase {

  void testRechnik1() {
    _ '''Власти московской управы "Крылатское" намерены в течение месяца решить вопрос о сносе
незаконных строений в поселке "Речник"''', """
intention
|who:
  authorities
  |of:
    council
    |part_of: Москва
    |name: Крылатское
|action:
  decide_on
  |time:
    deadline
    |unit: month
  |theme:
    demolition
    |of:
      set
      |component: building
      |legal: -
      |location:
        housing_development
        |name: Речник
"""
  }

  def _(input, output) {
    assertEquals output.trim(), new Parser(new RussianLexicon()).parse(input).semantics(0).prettyPrint()
  }

}