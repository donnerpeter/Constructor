package cons4

import java.util.ArrayList
import java.util.LinkedHashSet
import java.util.LinkedHashMap
import cons4.constructions.happy
import java.util.HashSet
import cons4.constructions.hasHead
import cons4.constructions.enrich
import cons4.constructions.canUnify
import cons4.constructions.isPenetrable
import java.util.HashMap
import java.util.Collections

public data class ParsingState(
        val log: String = "",
        val mites: List<Set<Mite>> = ArrayList(),
        val creators: Creators = Creators(),
        private val active: Set<Mite> = HashSet(),
        private val bestConfigurations: List<CandidateSet> = listOf(CandidateSet(setOf()))
) {
  fun equals(o: Any?) = this === o
  fun hashCode() = System.identityHashCode(this)

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

  fun findContradictors(mite: Mite, among: Collection<Mite>) = among.filter { it == mite || contradict(mite, it) }

  private val contrCache = HashMap<Pair<Mite, Mite>, Boolean>()
  fun contradict(mite1: Mite, mite2: Mite): Boolean {
    return contrCache.getOrPut(mite1 to mite2) { _contradict(mite1, mite2) }
  }
  private fun _contradict(mite1: Mite, mite2: Mite): Boolean {
    if (mite1.contradicts(mite2)) return true

    for (group in getDirectPreconditionGroups(mite1)) {
      if (group.all { contradict(it, mite2) }) return true
    }

    for (group in getDirectPreconditionGroups(mite2)) {
      if (group.all { contradict(mite1, it) }) return true
    }

    return false
  }

  private fun getDirectPreconditionGroups(mite: Mite): List<List<Mite>> = mite.primaries.map { creators.getParents(it) }.filter { it.notEmpty() }

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

  private fun addMite(added: Mite): ParsingState {
    val newMites = ArrayList(mites)
    newMites[newMites.lastIndex] = LinkedHashSet(newMites[newMites.lastIndex] + added)
    return copy(mites = newMites)
  }

  private fun addMites(added: Iterable<Mite>): ParsingState {
    return added.fold(this) { state, mite -> state.addMite(mite).updateActive(mite) }
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

  private fun updateActive(addedMite: Mite): ParsingState {
    val window = 2
    var bestWeight = Integer.MAX_VALUE - window
    val newBest = LinkedHashSet<CandidateSet>()

    val allMites = getAllMites()
    val allContradictors = LinkedHashSet(findContradictors(addedMite, allMites))

    newBest.addAll(bestConfigurations.filter { conf -> allContradictors.any { it in conf.set } })

    val allFreeCandidates = allMites.filter { it !in allContradictors && findContradictors(it, allContradictors).notEmpty() }
    val allAffectedMites = allMites.filter { findContradictors(it, allFreeCandidates).notEmpty() } + allContradictors + addedMite

    val byDelta = LinkedHashMap<Delta, ArrayList<CandidateSet>>()
    for (set in bestConfigurations) {
      val delta = set.enlarge(addedMite, this, allAffectedMites, allContradictors, allFreeCandidates)
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
      var state = _state.copy(mites = _state.mites + arrayListOf(LinkedHashSet())).addMites(cxts.toList())
      var toEnrich = cxts.toList()
      while (toEnrich.notEmpty()) {
        while (toEnrich.notEmpty()) {
          var newCreators = state.creators
          val enriched = LinkedHashSet<Mite>()
          val allMites = state.getAllMites()
          for (creator in toEnrich) {
            for (created in enrich(state, creator)) {
              newCreators = newCreators.addRelation(creator, created)
              if (created !in allMites) enriched.add(created)
            }
          }
          state = state.copy(creators = newCreators).addMites(enriched)
          toEnrich = enriched.toList()
        }

        while (true) {
          val merged = state.mergeMites()
          if (merged.isEmpty()) break
          toEnrich += merged
          var newCreators = merged.fold(state.creators) { creators, mite -> creators.addMergedMite(mite) }
          state = state.copy(creators = newCreators).addMites(merged)
        }
      }

      return state.appendLog(state.presentable() + "\n")
    }

  }

  fun toString() = "#${mites.lastIndex}"

}

data class Delta(
        val state: ParsingState,
        val addedMite: Mite,
        val freeCandidates: List<Mite>,
        val fixedMites: List<Mite>,
        val affectedMites : Set<Mite>,
        val weightOutside: Int
) {

  fun enumerateBestConfigurations(maxWeight: Int): List<List<Mite>> {
    val maxRemaining = maxWeight - weightOutside - fixedMites.count { !happy(it) }
    if (maxRemaining < 0) return listOf()
    return _enumerateBestConfigurations(fixedMites, freeCandidates.filter { happy(it) } + freeCandidates.filter { !happy(it) }, maxRemaining)
  }

  private fun _enumerateBestConfigurations(fixed: List<Mite>, freeMites: List<Mite>, maxUnhappy: Int): List<List<Mite>> {
    if (freeMites.empty) return listOf(fixed)

    val result = ArrayList<List<Mite>>()

    val head = freeMites[0]
    val tail = freeMites.subList(1, freeMites.size)

    val maxTailWeight = maxUnhappy - (if (happy(head)) 0 else 1)
    if (maxTailWeight >= 0 && state.findContradictors(head, fixed).empty) {
      result.addAll(_enumerateBestConfigurations(fixed + head, tail, maxTailWeight))
    }

    if (state.findContradictors(head, fixed + tail).notEmpty()) {
      result.addAll(_enumerateBestConfigurations(fixed, tail, maxUnhappy).filter { state.findContradictors(head, it).notEmpty() } )
    }

    return result
  }

}

data class CandidateSet(val set: Set<Mite>, val weight : Int = set.count { !happy(it) }) {

  fun enlarge(addedMite: Mite, state: ParsingState, allAffectedMites: List<Mite>, allExtruded: Set<Mite>, allFreeCandidates: List<Mite>): Delta {
    val extruded = LinkedHashSet(allExtruded.filter { it in set })
    val freeCandidates = allFreeCandidates.filter { it !in extruded && state.findContradictors(it, extruded).notEmpty() }
    val affectedMites = LinkedHashSet(allAffectedMites.filter { state.findContradictors(it, freeCandidates).notEmpty() } + extruded + addedMite)
    return Delta(state, addedMite, freeCandidates, set.filter { it !in extruded && it in affectedMites } + addedMite, affectedMites, weight - extruded.size + 1)
  }

  fun toString() = "$weight ${set.filter { !happy(it) }}"

}

data class Creators(val parents: Map<Mite, List<Mite>> = LinkedHashMap(), val children: Map<Mite, List<Mite>> = LinkedHashMap()) {
  fun addRelation(parent: Mite, child: Mite): Creators {
    val newParents = LinkedHashMap(parents)
    val newChildren = LinkedHashMap(children)
    newParents[child] = newParents.getOrElse(child) { listOf<Mite>() } + parent
    newChildren[parent] = newChildren.getOrElse(parent) { listOf<Mite>() } + child
    return copy(parents = newParents, children = newChildren)
  }

  fun getParents(mite: Mite): List<Mite> = parents[mite] ?: listOf()
  fun getChildren(mite: Mite): List<Mite> = children[mite] ?: listOf()

  fun addMergedMite(mite: Mite): Creators {
    val newParents = LinkedHashMap(parents)
    val newChildren = LinkedHashMap(children)
    for (child in LinkedHashSet(getChildren(mite.src1!!) + getChildren(mite.src2!!))) {
      newParents[child] = newParents.getOrElse(child) { listOf() } + mite
      newChildren[mite] = newChildren.getOrElse(mite) { listOf() } + child
    }
    return copy(parents = newParents, children = newChildren)
  }

}