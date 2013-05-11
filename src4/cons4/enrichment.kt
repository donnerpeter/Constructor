package cons4.enrichment

import cons4.constructions.*
import java.util.ArrayList
import cons4.Construction
import cons4.Variable

fun enrichMites(cxts: List<Construction>) = cxts.flatMap { enrich(it) }

fun enrich(cxt: Construction): List<Construction> {
  return when (cxt) {
    is word -> handleWord(cxt.word!!)
    else -> ArrayList()
  }
}

fun handleWord(w: String): List<Construction> {
  val v = Variable()
  return when (w) {
    "случай" -> listOf(nom(noun = v))
    "случился" -> listOf(nom(head = v))
    "удивительный" -> listOf(nom(noun = v.lightVar))
    else -> ArrayList()
  }
}