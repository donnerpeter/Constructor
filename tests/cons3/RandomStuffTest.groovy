package cons3

import junit.framework.TestCase

import static cons3.SonnetTest.doTranslateTest

/**
 * @author peter
 */
class RandomStuffTest extends TestCase {
  public void testIHaveMelon() {
    doTranslateTest 'У меня есть арбуз.',
            'I have a water melon.'
  }
  public void testIHadMelon() {
    doTranslateTest 'У меня был арбуз.',
            'I had a water melon.'
  }
  public void testVasyaHadMelon() {
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
  public void "фsafd"() {
    doTranslateTest 'Он увидел их',
            "To drink tea is to live long"
  }
  public void "фsafdвыф"() {
    doTranslateTest 'Он увидел их семью',
            "To drink tea is to live long"
  }
  public void "safd"() {
    doTranslateTest 'Он увидел их своими глазами',
            "To drink tea is to live long"
  }
  public void "safdы"() {
    doTranslateTest 'Он увидел их семью своими глазами',
            "To drink tea is to live long"
  }
  public void "safdыв"() {
    doTranslateTest 'Он велел ей помочь',
            "To drink tea is to live long"
  }
  public void "вsafdыв"() {
    doTranslateTest 'Он велел помочь ей',
            "To drink tea is to live long"
  }
  public void "_test I'm on the corner"() {
    doTranslateTest 'Я на углу',
            "I'm on the corner"
  }
  public void "_test I'm already on the corner of Znamenskaya and Basseynaya streets"() {
    doTranslateTest 'Я уже на углу Бассейной и Знаменской улицы',
            "I'm already on the corner of Znamenskaya and Basseynaya streets"
  }
}
