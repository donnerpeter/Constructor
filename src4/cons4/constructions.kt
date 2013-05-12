package cons4.constructions

import cons4.Construction
import cons4.Variable
import java.util.LinkedHashMap

data class word(val word: String? = null): Construction()
data class sem(val frame: Variable, val attr: String, val value: Any): Construction()

data class nom(val head: Variable? = null, val noun: Variable? = null): Construction()