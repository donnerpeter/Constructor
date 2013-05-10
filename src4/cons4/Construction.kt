package cons4

import java.util.LinkedHashMap
import java.lang.reflect.Field

open data class Construction {
  val name = this.javaClass.getSimpleName()

  private fun orderedFields() : List<Field> = javaClass.getDeclaredFields().sortBy { it.getName()!! }

  private fun toMap(): LinkedHashMap<String, Any?> {
    val result = LinkedHashMap<String, Any?>()
    for (field in orderedFields()) {
      result[field.getName()!!] = field.get(this)
    }
    return result
  }

  private fun mergeMaps(myMap: LinkedHashMap<String, Any?>, hisMap: LinkedHashMap<String, Any?>): LinkedHashMap<String, Any?>? {
    val merged = LinkedHashMap<String, Any?>()
    for ((key, myValue) in myMap) {
      val hisValue = hisMap[key]
      if (myValue != null && hisValue != null && myValue != hisValue) {
        return null
      }
      merged[key] = if (myValue != null) myValue else hisValue
    }
    return merged
  }

/*
  fun unify(cxt: Construction): Construction? {
    assert(cxt.javaClass == javaClass)
    val myMap = toMap()
    val mergedMap = mergeMaps(myMap, cxt.toMap())
    if (mergedMap == null) {
      return null
    }

    val list: List<Class<out Any?>> = orderedFields().map { it.getType() }
    val types: Array<Class<out Any?>> = list.toArray()
    val constructor = javaClass.getConstructor(types)
    val args = mergedMap.values().toArray()
    return constructor.newInstance(args)
  }
*/

}

class Mite(val cxt: Construction) {
  fun toString():String = cxt.name + "[]"
}