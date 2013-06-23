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
        val active: Set<Mite> = HashSet(),
        private val chosenColumns: List<CandidateSet> = listOf(),
        private val bestConfigurations: List<CandidateSet> = listOf(CandidateSet(setOf()))
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

  private fun enumerateHappierVariants(unhappyColumns: List<Int>): List<Map<Int, CandidateSet>> {
    val result = ArrayList<Map<Int, CandidateSet>>()
    fun doEnumerateHappierVariants(index: Int, map: Map<Int, CandidateSet>) {
      if (index == unhappyColumns.size) {
        result.add(map)
        return
      }

      val cIndex = unhappyColumns[index]
      val currentWeight = chosenColumns[cIndex].weight
      for (betterSet in network.columns[cIndex].candidateSets.filter { it.weight <= currentWeight }) {

      }
    }
    //    doEnumerateHappierVariants(0)
    return result
  }

  private fun getFreeCandidateSets(relatedColumns: List<Int>): Map<Int, List<CandidateSet>> {
    val activeOutside = active.filter { network.getAllIndices(it).any { it !in relatedColumns } }
    val result = HashMap<Int, List<CandidateSet>>()
    for (i in relatedColumns) {
      result[i] = network.columns[i].candidateSets.filter { it.set.all { network.findContradictors(it, activeOutside).empty } }
    }
    return result
  }

  private fun updateActive(addedMite: Mite): ParsingState {
    val touchedColumns = network.getAllIndices(addedMite)
    val relatedColumns = HashSet(touchedColumns.flatMap { network.getRelatedIndices(it) }).toSortedList()
    val unhappyColumns = relatedColumns.filter { chosenColumns[it].weight > 0 }
    val happierVariants = enumerateHappierVariants(unhappyColumns)

    val fixedColumns = HashSet<Int>()

    val newChosenColumns = chosenColumns

    val window = 2
    var bestWeight = Integer.MAX_VALUE - window
    val newBest = LinkedHashSet<CandidateSet>()

    val allMites = network.allMites
    val allContradictors = LinkedHashSet(findContradictors(addedMite, allMites))

    for (same in bestConfigurations.filter { conf -> allContradictors.any { it in conf.set } }) {
      newBest.add(same)
      bestWeight = Math.min(same.weight, bestWeight)
    }

    val allFreeCandidates = allMites.filter { it !in allContradictors && findContradictors(it, allContradictors).notEmpty() }
    val allAffectedMites = allMites.filter { findContradictors(it, allFreeCandidates).notEmpty() } + allContradictors + addedMite

    val byDelta = LinkedHashMap<Delta, ArrayList<CandidateSet>>()
    for (set in bestConfigurations) {
      val delta = set.enlarge(addedMite, network, allAffectedMites, allContradictors, allFreeCandidates)
      byDelta.getOrPut(delta) { ArrayList() }.add(set)
    }
    for ((delta, sets) in byDelta) {
      for (config in delta.enumerateBestConfigurations(bestWeight + window)) {
        for (set in sets) {
          val inertMites = set.set.filter { it !in delta.affectedMites }
          val better = CandidateSet(LinkedHashSet(inertMites + config))
          newBest.add(better)
          bestWeight = Math.min(better.weight, bestWeight)
        }
      }
    }

    val representedInConf = HashSet<Mite>()
    val sortedBest = ArrayList<CandidateSet>()
    for (conf in newBest.filter { it.weight <= bestWeight + window }.sortBy { it.weight }) {
      for (activeMite in conf.set) {
        if (representedInConf.add(activeMite)) {
          sortedBest.add(conf)
          break
        }
      }
    }
    return copy(bestConfigurations = sortedBest, active = sortedBest[0].set)
  }

  class object {
    fun _apply(_state: ParsingState, vararg cxts : Mite) : ParsingState {
      var state = _state.copy(network = _state.network.nextWord(), chosenColumns = _state.chosenColumns + CandidateSet(setOf())).addMites(cxts.toList())
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

data class Delta(
        val network: Network,
        val addedMite: Mite,
        val freeCandidates: List<Mite>,
        val fixedMites: List<Mite>,
        val affectedMites : Set<Mite>,
        val weightOutside: Int
) {

  fun enumerateBestConfigurations(maxWeight: Int): List<List<Mite>> {
    val maxRemaining = maxWeight - weightOutside - fixedMites.count { !it.happy }
    if (maxRemaining < 0) return listOf()
    return _enumerateBestConfigurations(fixedMites, freeCandidates.filter { it.happy } + freeCandidates.filter { !it.happy }, maxRemaining)
  }

  private fun _enumerateBestConfigurations(fixed: List<Mite>, freeMites: List<Mite>, maxUnhappy: Int): List<List<Mite>> {
    if (freeMites.empty) return listOf(fixed)

    val result = ArrayList<List<Mite>>()

    val head = freeMites[0]
    val tail = freeMites.subList(1, freeMites.size)

    val maxTailWeight = maxUnhappy - (if (head.happy) 0 else 1)
    if (maxTailWeight >= 0 && network.findContradictors(head, fixed).empty) {
      result.addAll(_enumerateBestConfigurations(fixed + head, tail, maxTailWeight))
    }

    if (network.findContradictors(head, fixed + tail).notEmpty()) {
      result.addAll(_enumerateBestConfigurations(fixed, tail, maxUnhappy).filter { network.findContradictors(head, it).notEmpty() } )
    }

    return result
  }

}

data class CandidateSet(val set: Set<Mite>, val weight : Int = set.count { !it.happy }) {

  fun enlarge(addedMite: Mite, network: Network, allAffectedMites: List<Mite>, allExtruded: Set<Mite>, allFreeCandidates: List<Mite>): Delta {
    val extruded = LinkedHashSet(allExtruded.filter { it in set })
    val freeCandidates = allFreeCandidates.filter { it !in extruded && network.findContradictors(it, extruded).notEmpty() }
    val affectedMites = LinkedHashSet(allAffectedMites.filter { network.findContradictors(it, freeCandidates).notEmpty() } + extruded + addedMite)
    val weightOutside = set.count { it !in affectedMites && !it.happy }
    val fixedMites = set.filter { it !in extruded && it in affectedMites } + addedMite
    return Delta(network, addedMite, freeCandidates, fixedMites, affectedMites, weightOutside)
  }

  fun toString() = "$weight ${set.filter { !it.happy }}"

  fun contradicts(another: CandidateSet, network: Network) = set.any { network.findContradictors(it, another.set).notEmpty() }

}
