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
  val v2 = Variable()
  return when (w) {
    "случай" -> listOf(nom(noun = v), sem(v, "type", "THING"))
    "случился" -> listOf(nom(head = v, noun = v2.lightVar), sem(v, "type", "HAPPEN"), sem(v, "arg1", v2))
    "удивительный" -> listOf(nom(noun = v.lightVar), sem(v, "property", "AMAZING"))
    else -> ArrayList()
  }
}