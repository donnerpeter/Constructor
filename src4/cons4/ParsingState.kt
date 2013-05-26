package cons4

import java.util.ArrayList
import java.util.LinkedHashSet
import java.util.LinkedHashMap
import cons4.constructions.happy
import java.util.HashSet
import cons4.constructions.hasHead
import cons4.constructions.enrich
import cons4.constructions.canUnify
import java.util.LinkedList

public data class ParsingState(
        val log: String = "",
        val mites: List<Set<Mite>> = ArrayList(),
        val creators: Map<Mite, Set<Mite>> = LinkedHashMap(),
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

  fun findContradictors(mite: Mite, among: Collection<Mite>) = among.filter { mite.contradicts(it) }

  fun calcMiteWeights(allMites: Collection<Mite>): Map<Mite, Double> {
    val miteWeights = LinkedHashMap<Mite, Double>()
    allMites.forEach { miteWeights[it] = 0.0 }
    allMites.forEach { mite ->
      if (!happy(mite)) {
        val contradictors = findContradictors(mite, allMites)
        for (contr in contradictors) {
          miteWeights[contr] = miteWeights[contr]!! + 1.0/contradictors.size.toDouble()
        }
      } else {
        miteWeights[mite] = miteWeights[mite]!! + 1
      }
    }
    return miteWeights
  }

  fun updateActive(): ParsingState {
    val allMites = getAllMites()
    val miteWeights = calcMiteWeights(allMites)

    val newActive = HashSet<Mite>()
    val processed = HashSet<Mite>()
    val queue = LinkedList(allMites.sortBy { -miteWeights[it]!! })
    while (queue.notEmpty()) {
      val mite = queue.removeFirst()
      if (mite in processed) continue

      if (findContradictors(mite, newActive).notEmpty()) {
        processed.add(mite)
        continue
      }

      val preconditions = LinkedHashSet<Mite>()
      var allCreatorsActive = true
      var hasChance = true
      for (atom in mite.primaries) {
        val creators = findPossibleAtomCreators(atom)
        if (creators.empty || creators.any { it in newActive }) continue
        allCreatorsActive = false
        val toProcess = creators.filter { it !in processed }
        if (toProcess.empty) hasChance = false
        preconditions.addAll(toProcess)
      }

      if (allCreatorsActive) {
        newActive.add(mite)
        processed.add(mite)
        continue
      }

      if (hasChance) {
        queue.addFirst(mite)
        queue.addAll(0, preconditions.sortBy { -miteWeights[it]!! })
      }
    }

    return copy(active = newActive)
  }

  private fun findPossibleAtomCreators(mite: Mite): List<Mite> {
    assert(mite.atom)
    val directCreators = creators[mite]
    if (directCreators == null) return listOf()
    return directCreators.toList()//getAllMites().filter { candidate -> directCreators.any { dc -> candidate.descendsFrom(dc) } }
  }

  fun presentable(): String {
    if (mites.empty) return ""

    var result = "${this}"

    val link = findPrevLink(mites.lastIndex)
    if (link != null) {
      result += " ${if (link.up) "/" else "-"}> #${link.prevIndex}                via ${link.mite}"
    }
    result += "\n"

    val map = LinkedHashMap<String, ArrayList<Mite>>()
    for (mite in mites.last!!) {
      map.getOrPut(mite.cxt.name) { ArrayList() }.add(mite)
    }

    for ((key, values) in map) {
      result += "  $key: " + values.map { (if (it in active) "*" else "") + (if (happy(it)) "" else "!") + it.args }.makeString(" ") + "\n"
    }

    val unhappy = getAllMites().filter { it in active && it !in mites.last!! && !happy(it) }
    if (unhappy.notEmpty()) {
      result += "\n  unhappy: " + unhappy.makeString(" ") + "\n"
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

  fun findPhraseStart(index: Int): Mite? {
    if (index < 0) return null

    val lastMites = mites[index]
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

    val lastMites = mites[index]
    val upLink = lastMites.find { it in active && hasHead(it) && !it.primaries.any { hasHead(it) && it in lastMites } }
    if (upLink != null) {
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
      result.addAll(mites[index])
    }

    val link = findPrevLink(index)
    if (link == null || !link.up && goingUp) return result

    result.addAll(getVisibleMites(link.prevIndex, true, true))
    return result
  }

  fun getAtomIndex(mite: Mite): Int {
    assert(mite.atom)
    for (i in 0..mites.lastIndex) {
      if (mite in mites[i]) {
        return i
      }
    }
    return -1
  }

  private fun mergeMites(): Set<Mite> {
    val result = LinkedHashSet<Mite>()
    val visible = getVisibleMites(mites.lastIndex, false)
    for (right in mites.last!!) {
      for (left in visible) {
        if (canUnify(left, right)) {
          val merged = left.unify(right)
          if (merged != null && merged !in mites.last!!) {
            result.add(merged)
          }
        }
      }
    }
    return result
  }

  class object {
    fun _apply(_state: ParsingState, vararg cxts : Mite) : ParsingState {
      var state = _state.copy(mites = _state.mites + arrayListOf(LinkedHashSet(cxts.toList()))).updateActive()
      var toEnrich = cxts.toList()
      while (toEnrich.notEmpty()) {
        while (toEnrich.notEmpty()) {
          val newCreators = LinkedHashMap(state.creators)
          val enriched = LinkedHashSet<Mite>()
          for (creator in toEnrich) {
            for (created in enrich(state, creator)) {
              newCreators[created] = LinkedHashSet(newCreators.getOrElse(created) { listOf<Mite>() } + creator)
              enriched.add(created)
            }
          }
          state = state.addMites(enriched).copy(creators = newCreators).updateActive()
          toEnrich = enriched.toList()
        }

        while (true) {
          val merged = state.mergeMites()
          if (merged.isEmpty()) break
          toEnrich += merged
          state = state.addMites(merged).updateActive()
        }
      }

      return state.appendLog(state.presentable() + "\n")
    }

  }

  fun toString() = "#${mites.lastIndex}"

}
