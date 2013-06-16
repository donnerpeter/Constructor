package cons4

import java.util.LinkedHashMap
import java.util.LinkedHashSet
import java.util.HashMap

data class Network(val parents: Map<Mite, List<Mite>> = LinkedHashMap(),
                   val children: Map<Mite, List<Mite>> = LinkedHashMap(),
                   private val contrCache: HashMap<Pair<Mite, Mite>, Boolean> = HashMap()) {
  fun addRelation(parent: Mite, child: Mite): Network {
    val newParents = LinkedHashMap(parents)
    val newChildren = LinkedHashMap(children)
    newParents[child] = newParents.getOrElse(child) { listOf<Mite>() } + parent
    newChildren[parent] = newChildren.getOrElse(parent) { listOf<Mite>() } + child
    return copy(parents = newParents, children = newChildren)
  }

  fun getParents(mite: Mite): List<Mite> = parents[mite].orEmpty()
  fun getChildren(mite: Mite): List<Mite> = children[mite].orEmpty()

  fun addMite(mite: Mite): Network {
    if (mite.src1 == null) return this
    return addMergedMite(mite)
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

