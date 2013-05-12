package cons4.enrichment

import cons4.constructions.*
import java.util.ArrayList
import cons4.Variable
import cons4.Mite
import cons4.Vars

fun enrichMites(cxts: List<Mite>) = cxts.flatMap { enrich(it) }

fun enrich(mite: Mite): List<Mite> {
  return when (mite.cxt) {
    word -> handleWord(mite["word"] as String)
    else -> ArrayList()
  }
}

fun handleWord(w: String): List<Mite> {
  val v = Vars()
  val vr = v["main"]
  return when (w) {
    "случай" -> listOf(nom("noun" to vr), sem.t(vr, "THING"))
    "случился" -> listOf(nom("head" to vr, "noun" to v[nom].lightVar), sem.t(vr, "HAPPEN"), sem(vr, "arg1", v[nom]))
    "удивительный" -> listOf(nom("noun" to vr.lightVar), sem(vr, "property", "AMAZING"))
    else -> ArrayList()
  }
}