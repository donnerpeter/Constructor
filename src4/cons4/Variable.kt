package cons4


public class Variable(base: Variable? = null, val comment: String = "") {
  val base = if (base == null) this else base
  private val id: String = if (base == null) "V${counter++}$comment" else "${base}_light"
  val lightVar = if (base == null) Variable(this) else this

  val hard: Boolean
    get() = base == this

  fun toString():String = id

  class object {
    var counter : Int = 0

    fun Variable(): Variable = Variable(null)
  }
}

