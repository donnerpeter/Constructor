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
  val vars = Vars()
  val vr = vars["main"]
  return when (w) {
    "мной" -> listOf(instr("noun" to vr), sem.t(vr, "ME"))
    "случай" -> listOf(nom("noun" to vr), sem.t(vr, "THING"))
    "случился" -> listOf(
            nom("head" to vr, "noun" to vars[nom].lightVar), sInstr("head" to vr, "noun" to vars[sInstr].lightVar),
            sem.t(vr, "HAPPEN"), sem(vr, "time", "PAST"), sem(vr, "arg1", vars[nom]), sem(vr, "experiencer", vars[sInstr]))
    "со" -> listOf(sInstr("noun" to vr), instr("noun" to vr.lightVar))
    "удивительный" -> listOf(nom("noun" to vr.lightVar), sem(vr, "property", "AMAZING"))
    else -> ArrayList()
  }
}