package cons4

import java.util.LinkedHashMap
import java.util.LinkedHashSet
import cons4.constructions.canUnify

open data class Construction {
  val name = this.javaClass.getSimpleName()

  fun invoke(args: List<Pair<String, Any>>) = Mite(this, linkedMapOf(*args.toArray(arrayOfNulls<Pair<String, Any>>(0) as Array<Pair<String, Any>>)))
  fun invoke(vararg args: Pair<String, Any>) = Mite(this, linkedMapOf(*args))

  fun toString() = name
}

data class Mite(val cxt: Construction, val args: LinkedHashMap<String, Any>, private val _primaries: List<Mite>? = null) {
  val primaries: LinkedHashSet<Mite> = LinkedHashSet(if (_primaries == null) listOf(this) else _primaries)

  fun has(vararg attrs: String) = attrs.all { args[it] != null }
  fun hasHard(vararg attrs: String) = attrs.all { args[it] is Variable && (args[it] as Variable).hard }
  fun v(attr: String) = args[attr] as Variable

  fun contradicts(another: Mite): Boolean {
    if (primaries.any { it in another.primaries }) return true

    val xor1 = this["xor"] as LinkedHashSet<Token>?
    val xor2 = another["xor"] as LinkedHashSet<Token>?
    if (xor1 == null || xor2 == null) return false

    return xor1.any { it in xor2 }
  }

  private fun mergeValues(key: String, myValue: Any?, hisValue: Any?): Any? {
    if (key == "xor") return unifyXor(myValue as LinkedHashSet<Token>?, hisValue as LinkedHashSet<Token>?)
    if (myValue is Variable && hisValue is Variable && (!myValue.hard || !hisValue.hard)) return Variable.mergeVars(myValue, hisValue)
    if (hisValue != null && myValue != null && myValue != hisValue) return null
    return myValue ?: hisValue
  }

  private fun mergeMaps(myMap: Map<String, Any>, hisMap: Map<String, Any>): LinkedHashMap<String, Any>? {
    val result = LinkedHashMap<String, Any>()
    for (key in LinkedHashSet(myMap.keySet() + hisMap.keySet())) {
      val merged = mergeValues(key, myMap[key], hisMap[key])
      if (merged == null) return null
      result.put(key, merged)
    }
    return result
  }

  fun unifyXor(xor1: Collection<Token>?, xor2: Collection<Token>?): Collection<Token>? {
    if (xor2 == null) return xor1
    if (xor1 == null) return xor2
    return LinkedHashSet(xor1 + xor2)
  }

  fun descendsFrom(mite: Mite) = primaries.containsAll(mite.primaries)

  fun unify(right: Mite): Mite? {
    if (cxt != right.cxt || !canUnify(this, right) || descendsFrom(right) || right.descendsFrom(this)) {
      return null
    }
    val myMap = args
    val mergedMap = mergeMaps(myMap, right.args)
    if (mergedMap == null) {
      return null
    }
    return copy(args = mergedMap, _primaries = this.primaries + right.primaries)
  }

  fun get(attr: String) = args[attr]

  val firstAtom: Mite get() = primaries.iterator().next()
  val atom: Boolean get() = primaries.size == 1

  fun toString() = cxt.name + args.toString()
}