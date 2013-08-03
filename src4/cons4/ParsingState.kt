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

  fun presentable(): String {
    if (network.lastIndex < 0) return ""

    var result = "${this}"

    val link = findPrevLink(network.lastIndex)
    if (link != null) {
      result += " ${if (link.up) "/" else "-"}> #${link.prevIndex}                via ${link.mite}"
    }
    result += "\n"

    val map = LinkedHashMap<String, ArrayList<Mite>>()
    for (mite in getVisibleMites(network.lastIndex, true)) {
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
    return added.fold(this) { state, mite -> state.copy(network = state.network.addMite(mite)) }.updateActive(added)
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
      return Link(getAtomIndex(headAtom), upLink, true)
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

  private fun enumerateVariantsWide(initialChange: ActiveChange, maxTotalWeight: Int): ActiveChange? {
    val queue = PriorityQueue<ActiveChange>()
    queue.offer(initialChange)
    while (queue.notEmpty()) {
      val change = queue.poll()!!
      if (change.pendingColumns.empty) return change

      val toAdd = change.suggestNextColumn()
      val maxAddedWeight = maxTotalWeight - change.weight
      val columnWeight = network.columns[toAdd].mites.count { !it.happy && it in active }
      val maxColumnWeight = if (toAdd == network.lastIndex) Integer.MAX_VALUE else columnWeight
      for (added in change.addColumn(toAdd, maxAddedWeight, maxColumnWeight)) {
        queue.offer(added)
      }
    }
    return null
  }

  private fun updateActive(addedMites: Iterable<Mite>): ParsingState {
    val trivialActive = HashSet(active)
    for (mite in addedMites) {
      if (network.findContradictors(mite, trivialActive, false).empty) {
        trivialActive.add(mite)
      }
    }
    return copy(active = trivialActive, network = network.copy(dirtyColumns = setOf())).improveActive(network.dirtyColumns)
  }

  private fun improveActive(dirtyColumns: Set<Int>): ParsingState {
    val relatedColumns = HashSet(dirtyColumns.flatMap { network.getRelatedIndices(it) })
    val unhappyColumns = relatedColumns.filter { network.columns[it].mites.any { !it.happy && it in active } }
    val startSet = unhappyColumns.toSortedList()

    val config = enumerateVariantsWide(ActiveChange(this, mapOf(), startSet.toSet(), startSet), active.count { !it.happy })
    if (config == null) {
      return this
    }

    val newActive = HashSet(active)
    for (idx in config.changes.keySet()) {
      newActive.removeAll(network.columns[idx].mites)
    }
    for (set in config.changes.values()) {
      newActive.addAll(set.set)
    }
    return copy(active = newActive)
  }

  class object {
    fun _apply(_state: ParsingState, vararg cxts : Mite) : ParsingState {
      var state = _state.copy(network = _state.network.nextWord()).addMites(cxts.toList())
      var toEnrich = cxts.toList()
      while (toEnrich.notEmpty()) {
        while (toEnrich.notEmpty()) {
          var newNetwork = state.network
          val newMites = LinkedHashSet<Mite>()
          val createdMites = LinkedHashSet<Mite>()
          val allMites = state.network.allMites
          for (creator in toEnrich) {
            for (created in enrich(state, creator)) {
              newNetwork = newNetwork.addRelation(setOf(creator), created)
              createdMites.add(created)
              if (created !in allMites) newMites.add(created)
            }
          }
          state = state.copy(network = newNetwork).addMites(newMites)
          toEnrich = createdMites.toList()
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

data class ActiveChange(val state: ParsingState, val changes: Map<Int, CandidateSet>, val pendingColumns: Set<Int>, val obligatoryColumns: List<Int>): Comparable<ActiveChange> {
  val chosenMites = HashSet(changes.values().flatMap { it.set })
  val weight = chosenMites.filter { !it.happy }.size()
  val activeOutside = state.active.count { mite -> !mite.happy && (changes.keySet() + pendingColumns).all { idx -> mite !in state.network.columns[idx].mites } }
  val minPendingWeight = pendingColumns.fold(0) { max, idx -> state.network.columns[idx].getMinimumWeight() }
  val estimate = weight + minPendingWeight + activeOutside

  public override fun compareTo(other: ActiveChange): Int {
    val diff = estimate - other.estimate
    if (diff != 0) return diff
    return pendingColumns.size() - other.pendingColumns.size()
  }

  fun suggestNextColumn() = obligatoryColumns.find { it !in changes.keySet() } ?: pendingColumns.toSortedList()[0]

  fun addColumn(idx: Int, maxAddedWeight: Int, maxColumnWeight: Int): List<ActiveChange> {
    val relatedColumns = state.network.getRelatedIndices(idx).filter { idx != it && !changes.containsKey(it) && !pendingColumns.contains(it) }

    val contradicting = state.network.columns[idx].mites.filter { mite -> state.network.findContradictors(mite, chosenMites, false).notEmpty() }.toSet()

    val result = ArrayList<ActiveChange>()
    for (set in state.network.columns[idx].candidateSets) {
      if (set.weight > maxColumnWeight) continue
      if (set.set.count { !it.happy && it !in chosenMites } > maxAddedWeight) continue

      if (set.set.all { it !in contradicting }) {
        val newChanges = HashMap(changes)
        newChanges[idx] = set

        val newPendingColumns = HashSet(pendingColumns)
        newPendingColumns.remove(idx)
        for (i in relatedColumns) {
          if (set.set.any { mite ->  state.network.findContradictors(mite, state.network.columns[i].mites, false).notEmpty() }) {
            newPendingColumns.add(i)
          }
        }

        result.add(copy(changes = newChanges, pendingColumns = newPendingColumns))
      }
    }
    return result
  }

}