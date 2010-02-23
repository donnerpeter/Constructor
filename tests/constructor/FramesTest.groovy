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
|agent:
  #1=authorities
  |governed:
    council
    |part_of: Москва
    |name: Крылатское
|goal:
  resolve_problem
  |agent: #1
  |time:
    deadline
    |unit: month
  |problem:
    demolition
    |undergoer:
      building
      |quantity: plural
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