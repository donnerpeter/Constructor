package cons4

import java.util.LinkedHashMap
import java.util.LinkedHashSet
import java.util.HashMap
import java.util.ArrayList
import java.util.HashSet

data class Network(val parents: Map<Mite, List<Set<Mite>>> = mapOf(),
                   val children: Map<Mite, List<Mite>> = mapOf(),
                   val mergeChildren: Map<Mite, List<Mite>> = mapOf(),
                   val mites: List<Set<Mite>> = listOf(),
                   val columns: List<Column> = listOf(),
                   val dirtyColumns: Set<Int> = setOf(),
                   private val contrCache: HashMap<Pair<Mite, Mite>, Boolean> = HashMap()) {
  fun equals(o: Any?) = this === o
  fun hashCode() = System.identityHashCode(this)

  val allMites: Set<Mite> get() = LinkedHashSet(mites.flatMap { it })
  val lastMites: Set<Mite> get() = if (mites.empty) setOf() else mites.last!!
  val lastIndex: Int get() = mites.lastIndex

  fun addRelation(parents: Set<Mite>, child: Mite): Network {
    val newParents = LinkedHashMap(this.parents)
    val newChildren = LinkedHashMap(children)
    newParents[child] = newParents.getOrElse(child) { listOf<Set<Mite>>() } + listOf(parents)
    for (parent in parents) {
      newChildren[parent] = newChildren.getOrElse(parent) { listOf<Mite>() } + child
    }
    val result = copy(parents = newParents, children = newChildren)
    if (child !in allMites) return result

    val subHierarchy = getSubHierarchy(child)
    return subHierarchy.fold(result) { net, touched -> net.updateColumns(touched) }
  }

  fun getSubHierarchy(mite: Mite): Set<Mite> {
    return LinkedHashSet(listOf(mite) + getChildren(mite).flatMap { getSubHierarchy(it) } + getMergeChildren(mite).flatMap { getSubHierarchy(it) })
  }
  fun getMergeHierarchy(mite: Mite): Set<Mite> {
    return LinkedHashSet(listOf(mite) + getMergeChildren(mite).flatMap { getMergeHierarchy(it) })
  }

  fun getParents(mite: Mite): List<Set<Mite>> = parents[mite].orEmpty()
  fun getChildren(mite: Mite): List<Mite> = children[mite].orEmpty()
  fun getMergeChildren(mite: Mite): List<Mite> = mergeChildren[mite].orEmpty()

  fun nextWord() = copy(mites = mites + arrayListOf(LinkedHashSet()), columns = columns + Column())

  fun addMite(mite: Mite): Network {
    val newMites = ArrayList(mites)
    newMites[newMites.lastIndex] = LinkedHashSet(newMites[newMites.lastIndex] + mite)
    var result = copy(mites = newMites)

    if (!mite.atom) {
      result = result.addMergedMite(mite, mite.src1!!, mite.src2!!)
    }
    return result.updateColumns(mite)
  }

  fun updateColumns(addedMite: Mite): Network {
    val newColumns = ArrayList(columns)
    var newDirtyColumns = dirtyColumns
    for (cIndex in getAllIndices(addedMite, false)) {
      newColumns[cIndex] = newColumns[cIndex].addMite(addedMite, this)
      if (cIndex !in newDirtyColumns) {
        newDirtyColumns = HashSet(newDirtyColumns + cIndex)
      }
    }
    return copy(columns = newColumns, dirtyColumns = newDirtyColumns)
  }

  fun getAllIndices(mite: Mite, includeParents: Boolean): List<Int> {
    val result = HashSet<Int>()
    if (!mite.atom) {
      result.addAll(getAllIndices(mite.src1!!, includeParents))
      result.addAll(getAllIndices(mite.src2!!, includeParents))
    } else {
      val atomIndex = getAtomIndex(mite)
      if (atomIndex >= 0) result.add(atomIndex)
    }
    if (includeParents) {
      for (parent in getParents(mite).flatMap { it }) {
        for (inheritor in getMergeHierarchy(parent)) {
          result.addAll(getAllIndices(inheritor, true))
        }
      }
    }
    return result.toSortedList()
  }

  private val relatedCache = HashMap<Int, List<Int>>()
  fun getRelatedIndices(column: Int): List<Int> {
    return relatedCache.getOrPut(column) { HashSet(columns[column].mites.flatMap { getAllIndices(it, true) }).toSortedList() }
  }

  fun getAtomIndex(mite: Mite): Int {
    assert(mite.atom)
    for (i in 0..lastIndex) {
      if (mite in mites[i]) {
        return i
      }
    }
    return -1
  }

  private fun addMergedMite(mite: Mite, src1: Mite, src2: Mite): Network {
    val newChildren = LinkedHashMap(children)
    for (child in LinkedHashSet(getChildren(src1) + getChildren(src2))) {
      newChildren[mite] = newChildren.getOrElse(mite) { listOf() } + child
    }
    val newMergeChildren = LinkedHashMap(mergeChildren)
    newMergeChildren[src1] = newMergeChildren.getOrElse(src1) { listOf() } + mite
    newMergeChildren[src2] = newMergeChildren.getOrElse(src2) { listOf() } + mite
    return copy(children = newChildren, mergeChildren = newMergeChildren)
  }

  fun findContradictors(mite: Mite, among: Collection<Mite>, includeSelf: Boolean) = among.filter { includeSelf && it == mite || contradict(mite, it) }

  fun contradict(mite1: Mite, mite2: Mite): Boolean {
    return contrCache.getOrPut(mite1 to mite2) { _contradict(mite1, mite2, true) }
  }
  private fun _contradict(mite1: Mite, mite2: Mite, checkCommonPrimaries: Boolean): Boolean {
    if (mite1 == mite2) return false
    if (checkCommonPrimaries && mite1.hasCommonPrimaries(mite2)) return true
    if (mite1.contradictsByXor(mite2)) return true

    for (atom in mite1.primaries) {
      val parentSets = getParents(atom)
      if (parentSets.notEmpty() && parentSets.all { set -> set.any { _contradict(it, mite2, false) } }) return true
    }
    for (atom in mite2.primaries) {
      val parentSets = getParents(atom)
      if (parentSets.notEmpty() && parentSets.all { set -> set.any { _contradict(mite1, it, false) } }) return true
    }

    return false
  }

}

data class Column(val mites: Set<Mite> = setOf(), val candidateSets: List<CandidateSet> = listOf(CandidateSet(setOf()))) {

  fun getMinimumWeight() = candidateSets[0].weight

  fun addMite(addedMite: Mite, network: Network): Column {
    if (addedMite in mites) return this

    val allMites = LinkedHashSet(mites + addedMite)
    val allContradictors = LinkedHashSet(network.findContradictors(addedMite, allMites, true))
    val allFreeCandidates = allMites.filter { it !in allContradictors && network.findContradictors(it, allContradictors, true).notEmpty() }
    val allAffectedMites = allMites.filter { network.findContradictors(it, allFreeCandidates, true).notEmpty() } + allContradictors + addedMite

    val newSets = LinkedHashSet<CandidateSet>()
    for (set in candidateSets) {
      if (allContradictors.any { it in set.set }) {
        newSets.add(set)
      }

      val delta = set.enlarge(addedMite, network, allAffectedMites, allContradictors, allFreeCandidates)
      val inertMites = set.set.filter { it !in delta.affectedMites }
      for (config in delta.enumerateBestConfigurations(Integer.MAX_VALUE)) {
        newSets.add(CandidateSet(LinkedHashSet(inertMites + config)))
      }
    }

    return copy(mites = allMites, candidateSets = newSets.sortBy { it.weight })
  }

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
    if (maxTailWeight >= 0 && network.findContradictors(head, fixed, true).empty) {
      result.addAll(_enumerateBestConfigurations(fixed + head, tail, maxTailWeight))
    }

    if (network.findContradictors(head, fixed + tail, true).notEmpty()) {
      result.addAll(_enumerateBestConfigurations(fixed, tail, maxUnhappy).filter { network.findContradictors(head, it, true).notEmpty() } )
    }

    return result
  }

}

data class CandidateSet(val set: Set<Mite>) {
  val weight : Int = set.count { !it.happy }

  fun enlarge(addedMite: Mite, network: Network, allAffectedMites: List<Mite>, allExtruded: Set<Mite>, allFreeCandidates: List<Mite>): Delta {
    val extruded = LinkedHashSet(allExtruded.filter { it in set })
    val freeCandidates = allFreeCandidates.filter { it !in extruded && network.findContradictors(it, extruded, true).notEmpty() }
    val affectedMites = LinkedHashSet(allAffectedMites.filter { network.findContradictors(it, freeCandidates, true).notEmpty() } + extruded + addedMite)
    val weightOutside = set.count { it !in affectedMites && !it.happy }
    val fixedMites = set.filter { it !in extruded && it in affectedMites } + addedMite
    return Delta(network, addedMite, freeCandidates, fixedMites, affectedMites, weightOutside)
  }

  fun toString() = "$weight ${set.filter { !it.happy }}"

}
