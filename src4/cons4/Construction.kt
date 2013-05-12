package cons4

import java.util.LinkedHashMap
import java.lang.reflect.Field

open data class Construction {
  val name = this.javaClass.getSimpleName()

  private fun orderedFields() : List<Field> = javaClass.getDeclaredFields().sortBy { it.getName()!! }

  fun toMap(): LinkedHashMap<String, Any?> {
    val result = LinkedHashMap<String, Any?>()
    for (field in orderedFields()) {
      field.setAccessible(true)
      result[field.getName()!!] = field.get(this)
    }
    return result
  }

  private fun mergeMaps(myMap: Map<String, Any?>, hisMap: Map<String, Any?>): LinkedHashMap<String, Any?>? {
    val merged = LinkedHashMap<String, Any?>()
    for ((key, myValue) in myMap) {
      val hisValue = hisMap[key]
      if (myValue is Variable && hisValue is Variable && (!myValue.hard || !hisValue.hard)) {
        merged[key] = Variable.mergeVars(myValue, hisValue)
      } else if (myValue != null && hisValue != null && myValue != hisValue) {
        return null
      } else {
        merged[key] = if (myValue != null) myValue else hisValue
      }
    }
    return merged
  }

  fun unify(cxt: Construction): Construction? {
    if (cxt.javaClass != javaClass) {
      return null
    }
    val myMap = toMap()
    val mergedMap = mergeMaps(myMap, cxt.toMap())
    if (mergedMap == null) {
      return null
    }
    return copyWithArgs(mergedMap)
  }

  fun copyWithArgs(args : Map<String, Any?>): Construction {
    val classList: List<Class<out Any?>> = orderedFields().map { it.getType()!! }
    val classArray: Array<Class<out Any?>> = classList.toArray(arrayOfNulls<Class<out Any?>>(0) as Array<Class<*>>)
    val constructor = javaClass.getConstructor(*classArray)
    val args = args.values().toArray()
    return constructor.newInstance(*args)!!

  }

}
