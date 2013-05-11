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
    var state = this.copy(mites = mites + arrayListOf(ArrayList()))
    var added = cxts.toList()
    while (added.notEmpty()) {
      state = state.addMites(added)
      val merged = state.mergeMites(added)
      state = state.addMites(merged)
      added = enrichMites(added + merged)
    }
    return state.appendLog(" ${state.mites.last}\n")
  }

  private fun addMites(added: List<Construction>): ParsingState {
    if (mites.empty) {
      return this
    }

    val newMites = ArrayList(mites)
    newMites[newMites.lastIndex] += added
    return copy(mites = newMites)
  }

  private fun mergeMites(newMites: List<Construction>): List<Construction> {
    val result: ArrayList<Construction> = ArrayList()
    if (mites.size <= 1) return result

    for (right in newMites) {
      val visible = mites[mites.lastIndex - 1]
      for (left in visible) {
        val merged = left.unify(right)
        if (merged != null) {
          result.add(merged)
        }
      }
    }
    return result
  }

}
