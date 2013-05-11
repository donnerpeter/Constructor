package cons4.constructions

import cons4.Construction
import cons4.Variable
import java.util.LinkedHashMap

data class word(val word: String? = null): Construction()

data class nom(val noun: Variable? = null, val head: Variable? = null): Construction() {
  override fun copyWithArgs(args : Map<String, Any?>): Construction {
    return copy(noun=args["noun"] as Variable?, head=args["head"] as Variable?)
  }
}

