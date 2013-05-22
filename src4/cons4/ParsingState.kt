package cons4

import java.util.ArrayList
import java.util.LinkedHashSet
import java.util.LinkedHashMap
import cons4.constructions.happy
import java.util.HashSet
import java.util.HashMap
import cons4.constructions.showMoreMites
import cons4.constructions.enrich

public data class ParsingState(
        val log: String = "",
        val mites: List<Set<Mite>> = ArrayList(),
        private val active: Set<Mite> = HashSet()
  ) {

  fun getChart(): Chart {
    return Chart(getActiveMites())
  }

  fun appendLog(newLog : String) : ParsingState {
    return copy(log = log + newLog)
  }

  fun printLog() {
    println("Log:\n\n$log\n")
  }

  fun apply(vararg cxts : Mite) = _apply(this, *cxts)

  fun getAllMites(): LinkedHashSet<Mite> = LinkedHashSet(mites.flatMap { it })
  fun getActiveMites(): LinkedHashSet<Mite> = LinkedHashSet(getAllMites().filter { it in active })

  fun contradicts(mite1: Mite, mite2: Mite) = mite1.primaries.any { it in mite2.primaries }
  fun findContradictors(mite: Mite, among: Collection<Mite>) = among.filter { contradicts(mite, it) }

  fun unhappyCount(mites: Collection<Mite>) = mites.filter { !happy(it) }.size

  fun updateActive(): ParsingState {
    val allMites = getAllMites()

    val byWeight = HashMap<Int, ArrayList<Mite>>()
    allMites.forEach { mite ->
      val weight = findContradictors(mite, allMites).filter { !happy(it) }.size + (if (happy(mite)) 1 else 0)
      byWeight.getOrPut(weight, { ArrayList<Mite>() }).add(mite)
    }

    val newActive = HashSet<Mite>()
    byWeight.keySet().toSortedList().reverse().forEach {
      byWeight[it]!!.forEach { mite ->
        if (findContradictors(mite, newActive).empty) {
          newActive.add(mite)
        }
      }
    }
    return copy(active = newActive)
  }

  fun presentable(): String {
    if (mites.empty) return ""

    val map = LinkedHashMap<String, ArrayList<Mite>>()
    for (mite in mites.last!!) {
      map.getOrPut(mite.cxt.name) { ArrayList() }.add(mite)
    }

    var result = ""
    for ((key, values) in map) {
      result += "  $key: " + values.map { (if (it in active) "*" else "") + (if (happy(it)) "" else "!") + it.args }.makeString(" ") + "\n"
    }
    return result
  }

  private fun addMites(added: Collection<Mite>): ParsingState {
    if (mites.empty) {
      return this
    }

    val newMites = ArrayList(mites)
    newMites[newMites.lastIndex] = LinkedHashSet(newMites[newMites.lastIndex] + added)
    return copy(mites = newMites)
  }

  fun getVisibleMites(index: Int): Set<Mite> {
    val lastMites = mites[index]
    val start = lastMites.filter { it in active }.map { getAtomIndex(it.firstAtom) }.fold(index) { i1, i2 -> Math.min(i1, i2) } - 1 //todo kotlin

    val result = LinkedHashSet<Mite>()
    if (start < 0) return result

    var part = mites[start].toList()
    while (part.notEmpty()) {
      result.addAll(part)
      part = part.filter { it in active }.flatMap { showMoreMites(it, this) }.filter { it !in result }
    }

    return result
  }

  fun getAtomIndex(atom: Mite): Int {
    for (i in 0..mites.lastIndex) {
      if (atom in mites[i]) {
        return i
      }
    }
    return -1
  }

  private fun mergeMites(): Set<Mite> {
    val result = LinkedHashSet<Mite>()
    if (mites.empty) return result

    val visible = getVisibleMites(mites.lastIndex)
    for (right in mites.last!!) {
      for (left in visible) {
        val merged = left.unify(right)
        if (merged != null && merged !in mites.last!!) {
          result.add(merged)
        }
      }
    }
    return result
  }

  class object {
    fun _apply(_state: ParsingState, vararg cxts : Mite) : ParsingState {
      var state = _state.copy(mites = _state.mites + arrayListOf(LinkedHashSet()))
      var added = cxts.toList()
      while (added.notEmpty()) {
        state = state.addMites(added).updateActive()

        while (true) {
          val merged = state.mergeMites()
          if (merged.isEmpty()) break
          added += merged
          state = state.addMites(merged).updateActive()
        }

        added = added.flatMap { enrich(it) }
      }

      return state.appendLog(state.presentable() + "\n")
    }

  }

  fun toString() = "#${mites.size}"

}
