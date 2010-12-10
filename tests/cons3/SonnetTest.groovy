package cons3

import junit.framework.TestCase

/**
 * @author peter
 */
class SonnetTest extends TestCase {

  public void testParse1() throws Exception {
    String input = "Удивительный случай случился со мной"
    String expected = """
A.property:=УДИВИТЕЛЬНЫЙ
A.type:=СЛУЧАЙ
B.type:=СЛУЧИТЬСЯ
this.time==PAST
B.theme:=A
C.type==Я
B.experiencer:=C
"""
    assertEquals expected.trim(), new Parser().parse(input)
  }
}
