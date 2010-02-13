package constructor

import constructor.russian.RussianLexicon

/**
 * @author peter
 */

class RussianDigressionTest extends GroovyTestCase {

  public void testMotherDaughter() throws Exception {
    _ "мать любит дочь", """
SubjPred #1=любит мать
Obj #1 дочь
"""
  }

  public void testMotherDaughter1() throws Exception {
    _ "мать любит родную дочь", """
SubjPred #1=любит мать
Obj #1 #2=дочь
AdjNoun родную #2
"""
  }

  public void testMotherDaughter2() throws Exception {
    _ "мать любит родная дочь", """
Obj #1=любит мать
SubjPred #1 #2=дочь
AdjNoun родная #2
"""
  }

  public void testMotherDaughter3() throws Exception {
    _ "родную мать любит дочь", """
AdjNoun родную #1=мать
Obj #2=любит #1
SubjPred #2 дочь
"""
  }

  def _(input, output) {
    assertEquals output.trim(), new Parser(new RussianLexicon()).parse(input).trim()
  }

}