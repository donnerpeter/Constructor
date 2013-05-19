package cons4

import java.util.LinkedHashMap
import java.util.LinkedHashSet
import cons4.constructions.canUnify

open data class Construction {
  val name = this.javaClass.getSimpleName()

  fun invoke(vararg args: Pair<String, Any>) = Mite(this, linkedMapOf(*args))

  fun toString() = name
}

data class Mite(val cxt: Construction, val args: LinkedHashMap<String, Any>, primaries: List<Mite>? = null) {
  val primaries: LinkedHashSet<Mite> = LinkedHashSet(if (primaries == null) listOf(this) else primaries)

  private fun mergeMaps(myMap: Map<String, Any>, hisMap: Map<String, Any>): LinkedHashMap<String, Any>? {
    val merged = LinkedHashMap<String, Any>()
    for ((key, myValue) in myMap) {
      val hisValue = hisMap[key]
      if (myValue is Variable && hisValue is Variable && (!myValue.hard || !hisValue.hard)) {
        merged[key] = Variable.mergeVars(myValue, hisValue)
      } else if (hisValue != null && myValue != hisValue) {
        return null
      } else {
        merged[key] = myValue
      }
    }
    for ((key, hisValue) in hisMap) {
      if (merged[key] == null) {
        merged[key] = hisValue
      }
    }
    return merged
  }

  fun unify(right: Mite): Mite? {
    if (cxt != right.cxt || !canUnify(this, right)) {
      return null
    }
    val myMap = args
    val mergedMap = mergeMaps(myMap, right.args)
    if (mergedMap == null) {
      return null
    }
    return copy(args = mergedMap, primaries = this.primaries + right.primaries)
  }

  fun get(attr: String) = args[attr]

  val firstAtom: Mite get() = primaries.iterator().next()

}