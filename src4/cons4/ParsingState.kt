package cons4

public data class ParsingState(
        val chart: Chart = Chart(),
        val log: String = ""
  ) {

  fun appendLog(newLog : String) : ParsingState {
    return copy(log = log + newLog)
  }

  fun printLog() {
    println("Log:\n\n$log\n")
  }

  fun apply(vararg cxts : Construction) : ParsingState {
    return appendLog("added ${cxts.toList()}\n")
  }

}