package cons3

import junit.framework.TestCase

import static cons3.SonnetTest.doTranslateTest

/**
 * @author peter
 */
class WhTest extends TestCase {
  public void "test what comes first 7 or 8"() {
    doTranslateTest "Что идёт раньше - 7 или 8?",
            "What comes first - 7 or 8?"
  }
  public void "test what do they think on this matter"() {
    doTranslateTest "Что они думают по этому поводу?",
            "What do they think on this matter?"
  }
  public void "_test what do they think on this matter"() {
    doTranslateTest "Кому она сломала челюсть?",
            "What do they think on this matter?"
  }
  public void "test what did she break"() {
    doTranslateTest "Что она ему сломала?",
            "What did she break?"
  }
  public void "___test what do they think on this matter"() {
    doTranslateTest "Кого он спросил?",
            "What do they think on this matter?"
  }
  public void "____test what do they think on this matter"() {
    doTranslateTest "О чём он спросил соседей?",
            "What do they think on this matter?"
  }

}
