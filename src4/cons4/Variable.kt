package cons4

import java.util.HashMap

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

  fun toString():String = "${if (hard) "V" else "v"}$comment"

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

class Vars {
  private var allocated = HashMap<Any, Variable>()

  fun get(key: Any): Variable {
    var result = allocated[key]
    if (result == null) {
      result = Variable()
      allocated[key] = result!!
    }
    return result!!
  }
}