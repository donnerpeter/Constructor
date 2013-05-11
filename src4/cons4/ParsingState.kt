package cons4

import java.util.ArrayList
import cons4.enrichment.*

public data class ParsingState(
        val chart: Chart = Chart(),
        val log: String = "",
        private val mites: List<List<Construction>> = ArrayList()
  ) {

  fun appendLog(newLog : String) : ParsingState {
    return copy(log = log + newLog)
  }

  fun printLog() {
    println("Log:\n\n$log\n")
  }

  fun apply(vararg cxts : Construction) : ParsingState {
    var state = this
    var added = cxts.toList()
    state = state.copy(mites = mites + arrayListOf(added))
    while (added.notEmpty()) {
      added = enrichMites(added)

      val newMites = ArrayList(state.mites)
      newMites[newMites.lastIndex] += added

      state = state.copy(mites = newMites)
    }
    return state.appendLog(" ${state.mites.last}\n")
  }

}
