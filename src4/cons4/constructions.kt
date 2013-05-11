package cons4.constructions

import cons4.Construction
import cons4.Variable

data class word(val word: String? = null): Construction()

data class nom(val noun: Variable? = null, val head: Variable? = null): Construction()

