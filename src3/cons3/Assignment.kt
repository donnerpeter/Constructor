package cons3

/**
 * @author peter
 */
data class Assignment<T>(
        val frame: T,
        val property: String,
        val value: Any,
        val generation: Int,
        val section: Int) {

  fun toString(): String = "${frameId(frame)}.$property=${stringValue()}"

  fun stringValue(): String = if (value is String) value else frameId(value)

  fun frameId(value: Any): String {
    return if (value is Variable) value.getId()!! else (value as Frame).getVar()!!.getId()!!
  }

  fun equals(o: Any): Boolean =
    o is Assignment<*> && o.frame == frame && o.property == property && o.value == value

  fun hashCode() : Int =
          (frame.hashCode() * 31 + property.hashCode()) * 31 + value.hashCode()
}
