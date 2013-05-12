package cons4.enrichment

import cons4.constructions.*
import java.util.ArrayList
import cons4.Construction
import cons4.Variable
import cons4.Mite

fun enrichMites(cxts: List<Mite>) = cxts.flatMap { enrich(it) }

fun enrich(mite: Mite): List<Mite> {
  return when (mite.cxt) {
    word -> handleWord(mite["word"] as String)
    else -> ArrayList()
  }
}

fun handleWord(w: String): List<Mite> {
  val v = Variable()
  val v2 = Variable()
  return when (w) {
    "случай" -> listOf(nom("noun" to v), sem.t(v, "THING"))
    "случился" -> listOf(nom("head" to v, "noun" to v2.lightVar), sem.t(v, "HAPPEN"), sem(v, "arg1", v2))
    "удивительный" -> listOf(nom("noun" to v.lightVar), sem(v, "property", "AMAZING"))
    else -> ArrayList()
  }
}