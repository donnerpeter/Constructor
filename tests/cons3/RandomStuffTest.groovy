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
  public void testVasyaHadMelon() throws Exception {
    doTranslateTest 'У Васи был арбуз.',
            'Vasya had a water melon.'
  }

  public void _testItsRaining() {
    doTranslateTest 'Идет дождь',
            "It's raining"
  }
  public void _testItsSnowing() {
    doTranslateTest 'Идет снег',
            "It's snowing"
  }

  public void _testToSmokeIsToDamageHealth() {
    doTranslateTest 'Курить - здоровью вредить',
            "To smoke is to damage one's health"
  }
  public void _testToDrinkTea() {
    doTranslateTest 'Чай пить - долго жить',
            "To drink tea is to live long"
  }
}
