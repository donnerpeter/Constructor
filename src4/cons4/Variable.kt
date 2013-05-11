package cons4

private var counter : Int = 0

public class Variable(
        hardBase: Variable? = null,
        private val comment: String = "${counter++}",
        val merge1: Variable? = null,
        val merge2: Variable? = null
) {
  val base = if (hardBase == null) this else hardBase
  val lightVar = if (hardBase == null) Variable(this, comment, merge1, merge2) else this

  val hard: Boolean
    get() = base == this

  fun toString():String = "V$comment${if (hard) "" else "_light"}"

  class object {

    fun resetCounter() {
      counter = 0
    }

    fun mergeVars(v1:Variable, v2:Variable): Variable {
      assert(!v1.hard || !v2.hard)
      val newVar = Variable(null, "${v1.comment}_${v2.comment}", v1, v2)
      return if (v1.hard || v2.hard) newVar else newVar.lightVar
    }
  }
}

