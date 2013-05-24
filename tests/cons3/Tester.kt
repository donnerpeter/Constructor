package cons3

import cons4.*
import junit.framework.Assert

public class Tester {

  public fun doTranslateTest(input: String, expected: String) {
    Variable.resetCounter()
    Tokens.counter = 0

    val state = Parser().parse(input)
    val chart: Chart
    try {
      chart = state.getChart()
    } catch(e: Throwable) {
      state.printLog()
      throw e
    }

    val actual : String
    try {
      actual = EnglishGenerator().generate(chart)!!
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

  public fun doParseTest(input: String, expected: String) {
    Variable.resetCounter()
    Tokens.counter = 0

    val state = Parser().parse(input)
    val chart: Chart
    try {
      chart = state.getChart()
    } catch(e: Throwable) {
      state.printLog()
      throw e
    }

    val actual = chart.presentable()
    if (actual != expected.trim()) {
      state.printLog()
    }

    Assert.assertEquals(expected.trim() + "\n", actual + "\n")

  }

}