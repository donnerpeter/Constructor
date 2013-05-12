package cons4

import java.util.ArrayList
import cons4.enrichment.*
import java.util.LinkedHashSet
import java.util.LinkedHashMap
import cons4.constructions.happy

public data class ParsingState(
        val log: String = "",
        private val mites: List<List<Mite>> = ArrayList(),
        private val mergeParents: Map<Mite, Set<Mite>> = LinkedHashMap(),
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

  fun apply(vararg cxts : Mite) : ParsingState {
    var state = this.copy(mites = mites + arrayListOf(ArrayList()))
    var added = cxts.toList()
    val totalAdded = LinkedHashSet<Mite>()
    while (added.notEmpty()) {
      totalAdded.addAll(added)
      state = state.addMites(added)

      val merged = state.mergeMites(added)
      totalAdded.addAll(merged.keySet())
      val newParents = LinkedHashMap(state.mergeParents)
      for ((mite, parents) in merged) {
        newParents[mite] = LinkedHashSet(listOf(parents.first, parents.second))
        newParents[parents.first] = LinkedHashSet<Mite>((newParents[parents.first] ?: listOf<Mite>()) + mite)
        newParents[parents.second] = LinkedHashSet<Mite>((newParents[parents.second] ?: listOf<Mite>()) + mite)
      }
      state = state.copy(mergeParents = newParents).addMites(merged.keySet())

      added = enrichMites(added + merged.keySet())
    }

    val initialActive = state.suggestActive(state.activeMites, totalAdded)
    val improved = state.improveActive(initialActive, totalAdded)
    state = state.copy(activeMites = improved)

    return state.appendLog(state.presentable() + "\n")
  }

  fun contradictors(mite: Mite) = mergeParents[mite] ?: listOf<Mite>()

  fun unhappyCount(mites: Collection<Mite>) = mites.filter { !happy(it) }.size

  fun improveActive(active: Set<Mite>, totalAdded: Collection<Mite>): Set<Mite> {
    var bestActive = active
    for (mite in totalAdded) {
      if (mite in bestActive && !happy(mite)) {
        val toRemove = contradictors(mite)
        for (contr in toRemove) {
          val frozen = LinkedHashSet(activeMites)
          frozen.removeAll(contradictors(contr))
          frozen.add(contr)
          val alternative = suggestActive(frozen, totalAdded)
          if (unhappyCount(alternative) < unhappyCount(bestActive)) {
            bestActive = alternative
          }
        }
      }
    }
    return bestActive
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

  private fun mergeMites(newMites: List<Mite>): Map<Mite, Pair<Mite, Mite>> {
    val result = LinkedHashMap<Mite, Pair<Mite, Mite>>()
    if (mites.size <= 1) return result

    for (right in newMites) {
      val visible = mites[mites.lastIndex - 1]
      for (left in visible) {
        val merged = left.unify(right)
        if (merged != null) {
          result.put(merged, Pair(left, right))
        }
      }
    }
    return result
  }

}
