package cons4

import java.util.ArrayList
import java.util.LinkedHashSet
import java.util.LinkedHashMap
import java.util.HashSet
import cons4.constructions.hasHead
import cons4.constructions.enrich
import cons4.constructions.canUnify
import cons4.constructions.isPenetrable
import java.util.HashMap
import java.util.Collections

public data class ParsingState(
        val log: String = "",
        val network: Network = Network(),
        val active: Set<Mite> = HashSet()
) {
  fun equals(o: Any?) = this === o
  fun hashCode() = System.identityHashCode(this)

  fun getChart() = Chart(LinkedHashSet(network.allMites.filter { it in active }))
  fun appendLog(newLog : String) = copy(log = log + newLog)

  fun printLog() = println("Log:\n\n$log\n")
  fun apply(vararg cxts : Mite) = _apply(this, *cxts)

  fun findContradictors(mite: Mite, among: Collection<Mite>) = network.findContradictors(mite, among)

  fun presentable(): String {
    if (network.lastIndex < 0) return ""

    var result = "${this}"

    val link = findPrevLink(network.lastIndex)
    if (link != null) {
      result += " ${if (link.up) "/" else "-"}> #${link.prevIndex}                via ${link.mite}"
    }
    result += "\n"

    val map = LinkedHashMap<String, ArrayList<Mite>>()
    for (mite in network.lastMites) {
      map.getOrPut(mite.cxt.name) { ArrayList() }.add(mite)
    }

    for ((key, values) in map) {
      result += "  $key: " + values.map { (if (it in active) "*" else "") + (if (it.happy) "" else "!") + it.args }.makeString(" ") + "\n"
    }

    val unhappy = network.allMites.filter { it in active && it !in network.lastMites && !it.happy }
    if (unhappy.notEmpty()) {
      result += "\n  unhappy: " + unhappy.makeString(" ") + "\n"
    }
    return result
  }

  private fun addMites(added: Iterable<Mite>): ParsingState {
    return added.fold(this) { state, mite -> state.copy(network = state.network.addMite(mite)).updateActive(mite) }
  }

  fun findPhraseStart(index: Int): Mite? {
    if (index < 0) return null

    val lastMites = network.mites[index]
    val allHeads = lastMites.filter { it in active && it.primaries.any { hasHead(it) && it in lastMites } }

    var earliestHead: Mite? = null
    for (head in allHeads) {
      val start = getAtomIndex(head.firstAtom)
      if (earliestHead == null || start < getAtomIndex(earliestHead!!)) earliestHead = head.firstAtom
    }
    if (earliestHead == null) return null

    val earliestStart = getAtomIndex(earliestHead!!)
    if (earliestStart == index) return null

    val childStart = findPhraseStart(earliestStart)
    return childStart ?: earliestHead
  }

  data class Link(val prevIndex: Int, val mite: Mite?, val up: Boolean)

  fun findPrevLink(index: Int): Link? {
    if (index <= 0) return null

    val lastMites = network.mites[index]
    val upLink = lastMites.find { it in active && hasHead(it) && !it.primaries.any { hasHead(it) && it in lastMites } }
    if (upLink != null && isPenetrable(upLink)) {
      val headAtom = upLink.primaries.find { hasHead(it) }!!
      return Link(getAtomIndex(headAtom), headAtom, true)
    }

    val phraseStart = findPhraseStart(index)
    if (phraseStart == null) {
      return if (index == 0) null else Link(index - 1, null, false)
    }

    return Link(getAtomIndex(phraseStart) - 1, phraseStart, false)
  }

  fun getVisibleMites(index: Int, includeStart: Boolean, goingUp: Boolean = false): Set<Mite> {
    val result = LinkedHashSet<Mite>()
    if (index < 0) return result

    if (includeStart) {
      result.addAll(network.mites[index])
    }

    val link = findPrevLink(index)
    if (link == null || !link.up && goingUp) return result

    result.addAll(getVisibleMites(link.prevIndex, true, true))
    return result
  }

  fun getAtomIndex(mite: Mite) = network.getAtomIndex(mite)

  private fun mergeMites(): Set<Mite> {
    val result = LinkedHashSet<Mite>()
    val visible = getVisibleMites(network.lastIndex, false)
    for (right in network.lastMites) {
      for (left in visible) {
        if (canUnify(left, right)) {
          val merged = left.unify(right)
          if (merged != null && merged !in network.lastMites) {
            result.add(merged)
          }
        }
      }
    }
    return result
  }

  private fun enumerateVariants(targetColumns: List<Int>, freeCandidateSets: Map<Int, List<CandidateSet>>, fixed: Map<Int, CandidateSet>, maxWeights: Map<Int, Int>): List<Map<Int, CandidateSet>> {
    val result = ArrayList<Map<Int, CandidateSet>>()
    fun doEnumerateHappierVariants(index: Int, map: Map<Int, CandidateSet>) {
      if (index == targetColumns.size) {
        result.add(map)
        return
      }

      val cIndex = targetColumns[index]
      val newActive = HashSet(map.values().flatMap { it.set })
      for (betterSet in freeCandidateSets[cIndex]!!.filter { it.weight <= maxWeights[cIndex]!! }) {
        if (betterSet.set.all { network.findContradictors(it, newActive).filter { mite -> mite != it }.empty }) {
          val mapPlus = LinkedHashMap(map)
          mapPlus[cIndex] = betterSet
          doEnumerateHappierVariants(index + 1, mapPlus)
        }
      }
    }
    doEnumerateHappierVariants(0, fixed)
    return result
  }

  private fun getFreeCandidateSets(relatedColumns: List<Int>): Map<Int, List<CandidateSet>> {
    val activeOutside = active.filter { network.getAllIndices(it).any { it !in relatedColumns } }
    val result = HashMap<Int, List<CandidateSet>>()
    for (i in relatedColumns) {
      result[i] = network.columns[i].candidateSets.filter { it.set.all { mite -> network.findContradictors(mite, activeOutside.filter {it != mite }).empty } }
    }
    return result
  }

  private fun updateActive(addedMite: Mite): ParsingState {
    val touchedColumns = network.dirtyColumns
    val relatedColumns = HashSet(touchedColumns.flatMap { network.getRelatedIndices(it) }).toSortedList()

    val maxWeights = HashMap<Int, Int>()
    for (i in relatedColumns) {
      maxWeights[i] = network.columns[i].mites.count { !it.happy && it in active } + (if (!addedMite.happy && i in touchedColumns) 1 else 0)
    }

    val unhappyColumns = relatedColumns.filter { maxWeights[it]!! > 0 }
    val happyColumns = relatedColumns.filter { maxWeights[it]!! == 0 }

    fun mapWeight(map: Map<Int, CandidateSet>) = map.values().fold(0) { sum, set -> sum + set.weight }

    val freeCandidateSets = getFreeCandidateSets(relatedColumns)
    val happierVariants = enumerateVariants(unhappyColumns, freeCandidateSets, linkedMapOf(), maxWeights).sortBy { mapWeight(it) }
    for (map in happierVariants) {
      val completeVariant = enumerateVariants(happyColumns, freeCandidateSets, map, maxWeights).sortBy { mapWeight(it) }.firstOrNull()
      if (completeVariant != null) {
        val newActive = HashSet(active)
        for (idx in completeVariant.keySet()) {
          newActive.removeAll(network.columns[idx].mites)
        }
        for (set in completeVariant.values()) {
          newActive.addAll(set.set)
        }
        return copy(active = newActive, network = network.copy(dirtyColumns = setOf()))
      }

    }

    throw AssertionError(addedMite)
  }

  class object {
    fun _apply(_state: ParsingState, vararg cxts : Mite) : ParsingState {
      var state = _state.copy(network = _state.network.nextWord()).addMites(cxts.toList())
      var toEnrich = cxts.toList()
      while (toEnrich.notEmpty()) {
        while (toEnrich.notEmpty()) {
          var newNetwork = state.network
          val enriched = LinkedHashSet<Mite>()
          val allMites = state.network.allMites
          for (creator in toEnrich) {
            for (created in enrich(state, creator)) {
              newNetwork = newNetwork.addRelation(creator, created)
              if (created !in allMites) enriched.add(created)
            }
          }
          state = state.copy(network = newNetwork).addMites(enriched)
          toEnrich = enriched.toList()
        }

        while (true) {
          val merged = state.mergeMites()
          if (merged.isEmpty()) break
          toEnrich += merged
          state = state.addMites(merged)
        }
      }

      return state.appendLog(state.presentable() + "\n")
    }

  }

  fun toString() = "#${network.lastIndex}"

}

