package cons4

import java.util.LinkedHashMap
import java.util.LinkedHashSet
import java.util.HashMap
import java.util.ArrayList
import java.util.HashSet

data class Network(val parents: Map<Mite, List<Mite>> = LinkedHashMap(),
                   val children: Map<Mite, List<Mite>> = LinkedHashMap(),
                   val mites: List<Set<Mite>> = ArrayList(),
                   val columns: List<Column> = ArrayList(),
                   private val contrCache: HashMap<Pair<Mite, Mite>, Boolean> = HashMap()) {
  fun equals(o: Any?) = this === o
  fun hashCode() = System.identityHashCode(this)

  val allMites: Set<Mite> get() = LinkedHashSet(mites.flatMap { it })
  val lastMites: Set<Mite> get() = if (mites.empty) setOf() else mites.last!!
  val lastIndex: Int get() = mites.lastIndex

  fun addRelation(parent: Mite, child: Mite): Network {
    val newParents = LinkedHashMap(parents)
    val newChildren = LinkedHashMap(children)
    newParents[child] = newParents.getOrElse(child) { listOf<Mite>() } + parent
    newChildren[parent] = newChildren.getOrElse(parent) { listOf<Mite>() } + child
    return copy(parents = newParents, children = newChildren)
  }

  fun getParents(mite: Mite): List<Mite> = parents[mite].orEmpty()
  fun getChildren(mite: Mite): List<Mite> = children[mite].orEmpty()

  fun nextWord() = copy(mites = mites + arrayListOf(LinkedHashSet()), columns = columns + Column())

  fun addMite(mite: Mite): Network {
    val newMites = ArrayList(mites)
    newMites[newMites.lastIndex] = LinkedHashSet(newMites[newMites.lastIndex] + mite)
    var result = copy(mites = newMites)

    if (!mite.atom) {
      result = result.addMergedMite(mite)
    }

    val newColumns = ArrayList(columns)
    for (cIndex in result.getAllIndices(mite)) {
      newColumns[cIndex] = newColumns[cIndex].addMite(mite, result)
    }
    return result.copy(columns = newColumns)
  }

  fun getAllIndices(mite: Mite): List<Int> {
    val result = HashSet<Int>()
    if (!mite.atom) {
      result.addAll(getAllIndices(mite.src1!!))
      result.addAll(getAllIndices(mite.src2!!))
    } else {
      val atomIndex = getAtomIndex(mite)
      if (atomIndex >= 0) result.add(atomIndex)
    }
    for (parent in getParents(mite)) {
      result.addAll(getAllIndices(parent))
    }
    return result.toSortedList()
  }

  fun getRelatedIndices(column: Int) = HashSet(columns[column].mites.flatMap { getAllIndices(it) }).toSortedList()

  fun getAtomIndex(mite: Mite): Int {
    assert(mite.atom)
    for (i in 0..lastIndex) {
      if (mite in mites[i]) {
        return i
      }
    }
    return -1
  }

  private fun addMergedMite(mite: Mite): Network {
    val newParents = LinkedHashMap(parents)
    val newChildren = LinkedHashMap(children)
    for (child in LinkedHashSet(getChildren(mite.src1!!) + getChildren(mite.src2!!))) {
      newParents[child] = newParents.getOrElse(child) { listOf() } + mite
      newChildren[mite] = newChildren.getOrElse(mite) { listOf() } + child
    }
    return copy(parents = newParents, children = newChildren)
  }

  fun findContradictors(mite: Mite, among: Collection<Mite>) = among.filter { it == mite || contradict(mite, it) }

  fun contradict(mite1: Mite, mite2: Mite): Boolean {
    return contrCache.getOrPut(mite1 to mite2) { _contradict(mite1, mite2) }
  }
  private fun _contradict(mite1: Mite, mite2: Mite): Boolean {
    if (mite1.contradicts(mite2)) return true

    for (atom in mite1.primaries) {
      val parents = getParents(atom)
      if (parents.notEmpty() && parents.all { contradict(it, mite2) }) return true
    }
    for (atom in mite2.primaries) {
      val parents = getParents(atom)
      if (parents.notEmpty() && parents.all { contradict(mite1, it) }) return true
    }

    return false
  }

}

data class Column(val mites: Set<Mite> = setOf(), val candidateSets: List<CandidateSet> = listOf(CandidateSet(setOf()))) {

  fun addMite(addedMite: Mite, network: Network): Column {
    val allMites = LinkedHashSet(mites + addedMite)
    val allContradictors = LinkedHashSet(network.findContradictors(addedMite, allMites))
    val allFreeCandidates = allMites.filter { it !in allContradictors && network.findContradictors(it, allContradictors).notEmpty() }
    val allAffectedMites = allMites.filter { network.findContradictors(it, allFreeCandidates).notEmpty() } + allContradictors + addedMite

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