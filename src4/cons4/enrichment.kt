package cons4.enrichment

import cons4.constructions.*
import java.util.ArrayList
import cons4.Variable
import cons4.Mite
import cons4.Vars

fun enrichMites(cxts: List<Mite>) = cxts.flatMap { enrich(it) }

fun enrich(mite: Mite): List<Mite> {
  if (mite.cxt == verb && (mite["verb"] as Variable?)?.hard == true) {
    return listOf(elaboration("head" to mite["verb"]!!))
  }

  return when (mite.cxt) {
    word -> handleWord(mite["word"] as String)
    else -> ArrayList()
  }
}

fun handleWord(w: String): List<Mite> {
  val v = Vars()
  val v0 = v[0]
  return when (w) {
    "вдруг" -> listOf(verb("verb" to v0.lightVar)) + sem(v0, "manner" to "SUDDENLY")
    "забыл" -> listOf(verb("verb" to v0), nom("head" to v0, "noun" to v[nom].lightVar)) +
          sem(v0, "type" to "FORGET", "time" to "PAST", "arg1" to v[nom])
    "мной" -> listOf(instr("noun" to v0), sem.t(v0, "ME"))
    "случай" -> listOf(nom("noun" to v0), sem.t(v0, "THING"))
    "случился" -> listOf(verb("verb" to v0), nom("head" to v0, "noun" to v[nom].lightVar), sInstr("head" to v0, "noun" to v[sInstr].lightVar)) +
      sem(v0, "type" to "HAPPEN", "time" to "PAST", "arg1" to v[nom], "experiencer" to v[sInstr])
    "со" -> listOf(sInstr("noun" to v0), instr("noun" to v0.lightVar, "head" to Any()))
    "удивительный" -> listOf(nom("noun" to v0.lightVar), sem(v0, "property", "AMAZING"))
    ":" -> listOf(elaboration("hasColon" to "true"))
    "я" -> listOf(nom("noun" to v0), sem.t(v0, "ME"))
    else -> ArrayList()
  }
}