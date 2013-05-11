package cons4

import java.util.ArrayList
import cons4.enrichment.*
import cons4.constructions.*
import java.util.LinkedHashSet
import java.util.LinkedHashMap

public data class ParsingState(
        val log: String = "",
        private val mites: List<List<Construction>> = ArrayList()
  ) {

  fun getChart(): Chart {
    val assignments = ArrayList<Assignment>()
    for (mite in getActiveMites()) {
      if (mite is sem) {
        assignments.add(Assignment(mite.frame, mite.attr, mite.value))
      }
    }
    return Chart(assignments)
  }

  fun getActiveMites(): LinkedHashSet<Construction> {
    val result = LinkedHashSet<Construction>()
    result.addAll(mites.flatMap { it })
    return result
  }

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
    return state.appendLog(state.presentable() + "\n")
  }

  fun presentable(): String {
    if (mites.empty) return ""

    val map = LinkedHashMap<String, ArrayList<Construction>?>()
    for (mite in mites.last!!) {
      var list = map[mite.name]
      if (list == null) {
        list = ArrayList()
        map[mite.name] = list
      }
      list!!.add(mite) //todo kotlin remove !!
    }

    var result = ""
    for ((key, values) in map) {
      result += "  $key: "
      for (mite in values!!) {
        result += mite.toMap().toString() + " "
      }
      result += "\n"
    }
    return result
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
