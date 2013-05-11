package cons4.enrichment

import cons4.constructions.*
import java.util.ArrayList
import cons4.Construction
import cons4.Variable

fun enrichMites(cxts: List<Construction>) = cxts.flatMap { enrich(it) }

fun enrich(cxt: Construction): List<Construction> {
  if (cxt is nom && cxt.head != null && cxt.noun != null) {
    return listOf(sem(cxt.head, "arg1", cxt.noun))
  }

  return when (cxt) {
    is word -> handleWord(cxt.word!!)
    else -> ArrayList()
  }
}

fun handleWord(w: String): List<Construction> {
  val v = Variable()
  return when (w) {
    "случай" -> listOf(nom(noun = v), sem(v, "type", "THING"))
    "случился" -> listOf(nom(head = v), sem(v, "type", "HAPPEN"))
    "удивительный" -> listOf(nom(noun = v.lightVar), sem(v, "property", "AMAZING"))
    else -> ArrayList()
  }
}