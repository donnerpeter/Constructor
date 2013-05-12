package cons4

import java.util.ArrayList
import cons4.enrichment.*
import java.util.LinkedHashSet
import java.util.LinkedHashMap
import cons4.constructions.happy

public data class ParsingState(
        val log: String = "",
        private val mites: List<List<Mite>> = ArrayList(),
        private val activeMites: Set<Mite> = LinkedHashSet()
  ) {

  fun getChart(): Chart {
    return Chart(activeMites)
  }

  fun appendLog(newLog : String) : ParsingState {
    return copy(log = log + newLog)
  }

  fun printLog() {
    println("Log:\n\n$log\n")
  }

  fun apply(vararg cxts : Mite) = _apply(this, *cxts)

  fun getAllMites(): LinkedHashSet<Mite> {
    val result = LinkedHashSet<Mite>()
    result.addAll(mites.flatMap { it })
    return result
  }

  fun contradictors(mite: Mite) = getAllMites().filter { it.primaries.any { it in mite.primaries } }

  fun unhappyCount(mites: Collection<Mite>) = mites.filter { !happy(it) }.size

  fun improveActive(active: Set<Mite>, totalAdded: Collection<Mite>): Set<Mite> {
    var bestActive = active
    while (true) {
      var changed = false
      for (mite in active) {
        if (mite in bestActive && !happy(mite)) {
          val toRemove = contradictors(mite)
          for (contr in toRemove) {
            val frozen = LinkedHashSet(activeMites)
            frozen.removeAll(contradictors(contr))
            frozen.add(contr)
            val alternative = suggestActive(frozen, totalAdded)
            if (unhappyCount(alternative) < unhappyCount(bestActive)) {
              bestActive = alternative
              changed = true
            }
          }
        }
      }
      if (!changed) {
        return bestActive
      }
    }
  }

  fun suggestActive(frozen: Collection<Mite>, free: Collection<Mite>): Set<Mite> {
    val result = LinkedHashSet<Mite>(frozen)
    for (mite in free) {
      if (contradictors(mite).all { it !in result }) {
        result.add(mite)
      }
    }
    return result
  }

  fun presentable(): String {
    if (mites.empty) return ""

    val map = LinkedHashMap<String, ArrayList<Mite>?>()
    for (mite in mites.last!!) {
      var list = map[mite.cxt.name]
      if (list == null) {
        list = ArrayList()
        map[mite.cxt.name] = list
      }
      list!!.add(mite) //todo kotlin remove !!
    }

    var result = ""
    for ((key, values) in map) {
      result += "  $key: "
      for (mite in values!!) {
        result += (if (mite in activeMites) "*" else "") + "${mite.args} "
      }
      result += "\n"
    }
    return result
  }

  private fun addMites(added: Collection<Mite>): ParsingState {
    if (mites.empty) {
      return this
    }

    val newMites = ArrayList(mites)
    newMites[newMites.lastIndex] += added.toList()
    return copy(mites = newMites)
  }

  private fun mergeMites(newMites: List<Mite>): Set<Mite> {
    val result = LinkedHashSet<Mite>()
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

  class object {
    fun _apply(_state: ParsingState, vararg cxts : Mite) : ParsingState {
      var state = _state.copy(mites = _state.mites + arrayListOf(ArrayList()))
      var added = cxts.toList()
      val totalAdded = LinkedHashSet<Mite>()
      while (added.notEmpty()) {
        totalAdded.addAll(added)
        state = state.addMites(added)

        val merged = state.mergeMites(added)
        totalAdded.addAll(merged)
        state = state.addMites(merged)

        added = enrichMites(added + merged)
      }

      val initialActive = state.suggestActive(state.activeMites, totalAdded)
      val improved = state.improveActive(initialActive, totalAdded)
      state = state.copy(activeMites = improved)

      return state.appendLog(state.presentable() + "\n")
    }

  }

}
