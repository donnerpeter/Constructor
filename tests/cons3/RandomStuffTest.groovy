package cons3

import junit.framework.TestCase

import static cons3.SonnetTest.doTranslateTest

/**
 * @author peter
 */
class RandomStuffTest extends TestCase {
  public void testIHaveMelon() throws Exception {
    doTranslateTest 'У меня есть арбуз.',
            'I have a water melon.'
  }
  public void testIHadMelon() throws Exception {
    doTranslateTest 'У меня был арбуз.',
            'I had a water melon.'
  }
}
