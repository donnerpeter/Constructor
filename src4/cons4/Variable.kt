package cons4

import java.util.HashMap
import java.util.LinkedHashSet

private var counter : Int = 0

public class Variable(
        hardBase: Variable? = null,
        private val comment: String = "${counter++}",
        primaries: Set<Variable>? = null
) {
  val base : Variable = if (hardBase == null) this else hardBase
  val lightVar : Variable = if (hardBase == null) Variable(this, comment, primaries) else this
  val primaries : Set<Variable> = if (primaries == null) LinkedHashSet(listOf(base)) else primaries

  fun hashCode(): Int = comment.hashCode()
  fun equals(o: Any): Boolean {
    if (o is Variable) {
      return if (primaries.size() == 1) o === this else primaries == o.primaries
    }
    return false
  }

  val hard: Boolean get() = base == this

  fun toString():String = "${if (hard) "V" else "v"}$comment"

  fun hardPrimary(): Variable? = primaries.find { it.hard }

  class object {

    fun resetCounter() {
      counter = 0
    }

    fun mergeVars(v1:Variable, v2:Variable): Variable {
      assert(!v1.hard || !v2.hard)
      val newVar = Variable(null, "${v1.comment}_${v2.comment}", LinkedHashSet(v1.primaries + v2.primaries))
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