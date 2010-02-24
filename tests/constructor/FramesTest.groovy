package constructor

import constructor.russian.RussianLexicon

 /**
 * @author peter
 */

class FramesTest extends GroovyTestCase {

  void testRechnik1() {
    _ '''
Власти московской управы "Крылатское" намерены в течение месяца решить вопрос о сносе
незаконных строений в поселке "Речник".

"Мы планируем все незаконные постройки снести за месяц", - сообщил журналистам во вторник
в поселке глава управы "Крылатское" Виталий Никитин.
''', """
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
tell
|message:
  intention
  |agent: #2=we
  |goal:
    demolition
    |agent: #2
    |undergoer:
      building
      |quantity: plural
      |scope: all
      |legal: -
    |time:
      deadline
      |unit: month
|addressee:
  journalist
  |quantity: plural
|time: Tuesday
|location: housing_development
|agent:
  head
  |governed:
    council
    |name: Крылатское
  |name:
    HumanName
    |first: Виталий
    |last: Никитин
"""
  }

  def _(input, output) {
    assertEquals output.trim(), new Parser(new RussianLexicon()).parse(input).semantics()
  }

}