package cons4

import java.util.HashMap
import java.util.LinkedHashSet
import cons4.Variable.HardVariable
import cons4.Variable.MergedVariable

private var counter : Int = 0

fun Variable() = HardVariable()

abstract class Variable(private val comment: String) {
  abstract val base: Variable
  abstract val lv: LightVariable
  abstract val primaries: Set<Variable>
  abstract val hard: Boolean

  fun toString():String = "${if (hard) "V" else "v"}$comment"

  class HardVariable(comment: String = "${counter++}"): Variable(comment) {
    override val base: Variable get() = this
    override val primaries: Set<Variable> = LinkedHashSet(listOf(this))
    override val hard: Boolean get() = true
    override val lv = LightVariable(this)
  }

  class LightVariable(override val base: Variable): Variable(base.comment) {
    {
      assert(base.hard)
    }
    override val lv: Variable.LightVariable get() = this
    override val primaries: Set<Variable> get() = base.primaries
    override val hard: Boolean get() = false

    fun equals(o: Any) = o is LightVariable && o.base == base
    fun hashCode() = base.hashCode()
  }

  class MergedVariable(override val primaries: Set<Variable>) : Variable(primaries.map { it.comment }.makeString("_")) {
    override val lv = LightVariable(this)
    override val base: Variable get() = this
    override val hard: Boolean get() = true

    fun equals(o: Any?) = o is MergedVariable && o.primaries == primaries
    fun hashCode() = primaries.hashCode()
  }

  class object {

    fun resetCounter() {
      counter = 0
    }

    fun mergeVars(v1:Variable, v2:Variable): Variable {
      assert(!v1.hard || !v2.hard)
      val newVar = MergedVariable(LinkedHashSet(v1.primaries + v2.primaries))
      return if (v1.hard || v2.hard) newVar else newVar.lv
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