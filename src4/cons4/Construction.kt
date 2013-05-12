package cons4

import java.util.LinkedHashMap
import java.lang.reflect.Field

open data class Construction {
  val name = this.javaClass.getSimpleName()

  fun invoke(vararg args: Pair<String, Any>) = Mite(this, linkedMapOf(*args))

}

data class Mite(val cxt: Construction, val args: LinkedHashMap<String, Any>) {
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
    if (cxt != right.cxt) {
      return null
    }
    val myMap = args
    val mergedMap = mergeMaps(myMap, right.args)
    if (mergedMap == null) {
      return null
    }
    return copy(args = mergedMap)
  }

  fun get(attr: String) = args[attr]

}