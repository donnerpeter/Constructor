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

  private fun addMites(added: Collection<Mite>): ParsingState {
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

  private fun updateActive(addedMites: Collection<Mite>): ParsingState {
    val trivialActive = HashSet(active)
    for (mite in addedMites.filter { it.happy }) {
      if (network.findContradictors(mite, trivialActive, false).empty) {
        trivialActive.add(mite)
      }
    }
    for (mite in addedMites.filter { !it.happy }) {
      if (network.findContradictors(mite, trivialActive, false).empty) {
        trivialActive.add(mite)
      }
    }
    val cp = copy(active = trivialActive, network = network.copy(dirtyColumns = setOf(), dirtyMites = setOf()))
    
//    val unhappyChosen = LinkedHashSet(network.allMites.filter { !it.happy && it in trivialActive })
//    val unhappyNeighbors = LinkedHashSet(network.dirtyMites.filter { !it.happy } + network.dirtyMites.flatMap { network.getContradictors(it).filter { !it.happy && it in trivialActive } } )
//    val unhappyDirty = LinkedHashSet(network.dirtyMites.filter { !it.happy })
    val r1 = cp.improveActive(network.dirtyMites)
/*
    val r2 = cp.improveActive(if (unhappyDirty.notEmpty()) unhappyDirty else network.dirtyMites)
    val u1 = r1.active.count { !it.happy }
    val u2 = r2.active.count { !it.happy }
    if (u1 != u2) {
      val d1 = r1.active.filter { it !in r2.active }
      val d2 = r2.active.filter { it !in r1.active }
      println("!=")
      val a = 1
    }
*/
    return r1
  }

  private fun improveActive(dirtyMites: Set<Mite>): ParsingState {
    if (dirtyMites.empty) return this
    
    var best: ActiveChange? = null
    var bestWeight = active.count { !it.happy }

    val queue = PriorityQueue<ActiveChange>()
    queue.offer(ActiveChange(this, mapOf(), dirtyMites))
    while (queue.notEmpty()) {
      val change = queue.poll()!!
      if (bestWeight <= change.fixedWeight) {
        continue
      }
      if (change.pendingMites.empty) {
        if (bestWeight > change.totalWeight) {
          best = change
          bestWeight = change.totalWeight
        }
        continue
      }
      
      for (next in change.branch()) {
        queue.offer(next)
      }
    }
    
    if (best == null) {
      return this
    }
    
    val changeMap = best!!.fixed

    val newActive = HashSet(active)
    newActive.removeAll(changeMap.keySet())
    newActive.addAll(changeMap.keySet().filter { changeMap[it]!! })
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

data class ActiveChange(val state: ParsingState, val fixed: Map<Mite, Boolean>, val pendingMites: Set<Mite>): Comparable<ActiveChange> {
  val fixedWeight = fixed.keySet().count { !it.happy && fixed[it] == true }
  val totalWeight = if (pendingMites.empty) fixedWeight + state.active.count { !it.happy && !fixed.containsKey(it) } else 0

  public override fun compareTo(other: ActiveChange): Int {
    val diff = pendingMites.size() - other.pendingMites.size()
    if (diff != 0) return diff
    return fixedWeight - other.fixedWeight
  }
  
  fun branch(): List<ActiveChange> {
    val mite = pendingMites.iterator().next()
    
    val result = ArrayList<ActiveChange>()
    
    val takenFixed = HashMap(fixed)
    val takenPending = LinkedHashSet(pendingMites)
    if (markForAdd(state, mite, takenFixed, takenPending)) {
      result.add(ActiveChange(state, takenFixed, takenPending))
    } 

    val omitFixed = HashMap(fixed)
    val omitPending = LinkedHashSet(pendingMites)
    if (markForRemove(state, mite, omitFixed, omitPending)) {
      result.add(ActiveChange(state, omitFixed, omitPending))
    } 
    return result
  }

}

fun mustBeAdded(state: ParsingState, mite: Mite, fixed: Map<Mite, Boolean>): Boolean {
  return !fixed.containsKey(mite) && state.network.getContradictors(mite).count { !fixed.containsKey(it) } == 0
}
fun isLost(state: ParsingState, mite: Mite, fixed: Map<Mite, Boolean>): Boolean {
  return fixed[mite] == false && state.network.getContradictors(mite).all { fixed[it] == false }
}
fun isFreed(state: ParsingState, mite: Mite, fixed: Map<Mite, Boolean>, pending: Set<Mite>): Boolean {
  if (fixed.containsKey(mite) || mite in pending) return false
  val contradictors = state.network.getContradictors(mite)
  return contradictors.count { it in pending || fixed[it] ?: (it in state.active) } == 0
}

fun markForRemove(state: ParsingState, mite: Mite, fixed: MutableMap<Mite, Boolean>, pending: MutableSet<Mite>): Boolean {
  fixed[mite] = false
  pending.remove(mite)
  if (isLost(state, mite, fixed)) {
    return false
  }
  
  val contradictors = state.network.getContradictors(mite)
  for (c in contradictors) {
    if (isLost(state, c, fixed)) {
      return false
    }
    if (mustBeAdded(state, c, fixed)) {
      if (!markForAdd(state, c, fixed, pending)) {
        return false
      }
    }
    if (isFreed(state, c, fixed, pending)) {
      pending.add(c)
    }
  }
  if (!contradictors.any { fixed[it] == true }) {
    pending.addAll(contradictors.filter { !fixed.containsKey(it) })
  }
  return true
}

fun markForAdd(state: ParsingState, mite: Mite, fixed: MutableMap<Mite, Boolean>, pending: MutableSet<Mite>): Boolean {
  fixed[mite] = true
  pending.remove(mite)
  val contras = state.network.getContradictors(mite).filter { !fixed.containsKey(it) }
  for (c in contras) {
    fixed[c] = false
  }
  for (c in contras) {
    if (!markForRemove(state, c, fixed, pending)) {
      return false
    }
  }
  return true
}
