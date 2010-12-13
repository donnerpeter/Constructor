package cons3

import junit.framework.TestCase

/**
 * @author peter
 */
class SonnetTest extends TestCase {

  public void testParse1() throws Exception {
    String input = "Удивительный случай случился со мной: я вдруг забыл, что идет раньше - 7 или 8"
    String expected = """
A.property:=AMAZING
A.type:=THING
B.type:=HAPPEN
this.time==PAST
B.theme:=A
C.type==ME
B.experiencer:=C
this.elaboration:=#2
--
A.type==ME
B.manner:=SUDDENLY
B.experiencer:=A
B.type:=FORGET
this.time==PAST
B.theme:=#3
--
this.questioned:=A
B.type==COME_SCALARLY
this.time==PRESENT
B.theme==A
B.order==EARLIER
A.variant==7
A.variant==8
"""
    assertEquals expected.trim(), new Parser().parse(input).presentable()
  }
}
