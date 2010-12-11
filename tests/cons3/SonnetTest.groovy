package cons3

import junit.framework.TestCase

/**
 * @author peter
 */
class SonnetTest extends TestCase {

  public void testParse1() throws Exception {
    String input = "Удивительный случай случился со мной: я вдруг забыл, что идет раньше - 7 или 8"
    String expected = """
A.property:=УДИВИТЕЛЬНЫЙ
A.type:=СЛУЧАЙ
B.type:=СЛУЧИТЬСЯ
this.time==PAST
B.theme:=A
C.type==Я
B.experiencer:=C
--
A.type==Я
B.manner:=ВДРУГ
B.experiencer:=A
B.type:=ЗАБЫТЬ
this.time==PAST
--
this.questioned:=A
B.type==ИДТИ_В_ПОСЛЕДОВАТЕЛЬНОСТИ
this.time==PRESENT
B.theme==A
B.order==РАНЬШЕ
A.variant==7
A.variant==8
"""
    assertEquals expected.trim(), new Parser().parse(input)
  }
}
