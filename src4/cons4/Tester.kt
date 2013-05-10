package cons4

import junit.framework.Assert

public class Tester {

  public fun doTranslateTest(input: String, expected: String) {
    Variable.counter = 0
    Tokens.counter = 0

    val state = Parser().parse(input)
    val chart = state.chart

    val actual : String
    try {
      actual = EnglishGenerator().generate(chart)
    } catch (e : Throwable) {
      println("\nChart:\n\n${chart.presentable()}")
      state.printLog()
      throw e
    }

    if (actual != expected) {
      println("\nChart:\n\n${chart.presentable()}")
      state.printLog()
    }

    Assert.assertEquals(expected, actual)

  }

}