package cons4

import java.util.ArrayList
import java.util.LinkedHashSet
import java.util.LinkedHashMap
import java.util.PriorityQueue
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

  private fun enumerateVariantsWide(change: ActiveChange, maxWeight: Int): ActiveChange? {
    val queue = PriorityQueue<ActiveChange>()
    queue.offer(change)
    while (queue.notEmpty()) {
      val each = queue.poll()!!
      if (each.pendingColumns.empty) return each

      val toAdd = each.pendingColumns.toSortedList()[0]
      for (added in each.addColumn(toAdd, maxWeight)) {
        queue.offer(added)
      }
    }
    return null
  }
  private fun enumerateVariantsDeep(columns: List<Int>, change: ActiveChange, maxWeight: Int): ActiveChange? {
    if (columns.empty) return enumerateVariantsWide(change, maxWeight)

    var bestWeight = maxWeight
    var bestResult: ActiveChange? = null
    val tail = columns.subList(1, columns.size())
    for (withIdx in change.addColumn(columns[0], bestWeight)) {
      val result = enumerateVariantsDeep(tail, withIdx, bestWeight)
      if (result != null && (bestResult == null || result.weight < bestWeight)) {
        bestResult = result
        bestWeight = result.weight
      }
    }
    return bestResult
  }

  private fun updateActive(addedMite: Mite): ParsingState {
    val touchedColumns = network.dirtyColumns
    val relatedColumns = HashSet(touchedColumns.flatMap { network.getRelatedIndices(it) })
    val unhappyColumns = relatedColumns.filter { network.columns[it].mites.any { !it.happy && it in active } }
    val startSet = HashSet(touchedColumns + unhappyColumns).toSortedList()

    val maxWeight = active.count { !it.happy } + (if (!addedMite.happy && network.findContradictors(addedMite, active).empty) 1 else 0)

    val config = enumerateVariantsDeep(startSet, ActiveChange(this, mapOf(), setOf()), maxWeight)
    if (config == null) {
      throw RuntimeException("$addedMite")
    }

    val newActive = HashSet(active)
    for (idx in config.changes.keySet()) {
      newActive.removeAll(network.columns[idx].mites)
    }
    for (set in config.changes.values()) {
      newActive.addAll(set.set)
    }
    return copy(active = newActive, network = network.copy(dirtyColumns = setOf()))
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

data class ActiveChange(val state: ParsingState, val changes: Map<Int, CandidateSet>, val pendingColumns: Set<Int>): Comparable<ActiveChange> {
  val chosenMites = HashSet(changes.values().flatMap { it.set })
  val weight = chosenMites.filter { !it.happy }.size()

  public override fun compareTo(other: ActiveChange): Int {
    val diff = weight - other.weight
    if (diff != 0) return diff
    return pendingColumns.size() - other.pendingColumns.size()
  }

  fun addColumn(idx: Int, maxTotalWeight: Int): List<ActiveChange> {
    val relatedColumns = state.network.getRelatedIndices(idx).filter { idx != it && !changes.containsKey(it) && !pendingColumns.contains(it) }

    val result = ArrayList<ActiveChange>()
    for (set in state.network.columns[idx].candidateSets) {
      if (set.set.count { !it.happy && it !in chosenMites } > maxTotalWeight - weight) continue

      if (set.set.all { mite -> state.network.findContradictors(mite, chosenMites).filter { it != mite }.empty }) {
        val newChanges = HashMap(changes)
        newChanges[idx] = set

        val newPendingColumns = HashSet(pendingColumns)
        newPendingColumns.remove(idx)
        for (i in relatedColumns) {
          if (set.set.any { mite ->  state.network.findContradictors(mite, state.network.columns[i].mites).filter { it != mite }.notEmpty() }) {
            newPendingColumns.add(i)
            continue
          }
        }

        result.add(copy(changes = newChanges, pendingColumns = newPendingColumns))
      }
    }
    return result
  }

}