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
  public void "test whose jaw did she break"() {
    doTranslateTest "Кому она сломала челюсть?",
            "Whose jaw did she break?"
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
  public void "_____test what do they think on this matter"() {
    doTranslateTest "Я увидел дом, в котором они сидят",
            "What do they think on this matter?"
  }
  public void "______test what do they think on this matter"() {
    doTranslateTest "Я спросил про дом, что он увидел",
            "What do they think on this matter?"
  }
  public void "_____a_test what do they think on this matter"() {
    doTranslateTest "Кому он велел помочь?",
            "What do they think on this matter?"
  }
  public void "__ы___a_test what do they think on this matter"() {
    doTranslateTest "Кому он велел ей помочь?",
            "What do they think on this matter?"
  }
  public void "__ы___a_test what do вthey think on this matter"() {
    doTranslateTest "Кому он велел помочь, ей?",
            "What do they think on this matter?"
  }

}
