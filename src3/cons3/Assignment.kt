package cons3

/**
 * @author peter
 */
data class Assignment<T>(
        val frame: T,
        val property: String,
        val value: Any,
        generation: Int,
        section: Int) {
  val generation = generation
  val section = section

  fun toString(): String = "${frameId(frame)}.$property=${stringValue()}"

  fun stringValue(): String = if (value is String) value else frameId(value)

  fun frameId(value: Any): String {
    return if (value is Variable) value.getId()!! else (value as Frame).getVar()!!.getId()!!
  }

}
