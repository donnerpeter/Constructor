package cons4.enrichment

import cons4.constructions.*
import java.util.ArrayList
import cons4.Mite
import cons4.Vars
import cons4.Construction

fun handleWord(w: String): List<Mite> {
  val v = Vars()
  val v0 = v[0]
  return when (w) {
    "вдруг" -> l(verb("verb" to v0.lv)) + sem(v0, "manner" to "SUDDENLY")
    "забыл" -> finiteVerb(v, "PAST") + sem(v0, "type" to "FORGET")
    "идет" -> finiteVerb(v, "PRESENT") + l(comeScalarly("verb" to v0))
    "мной" -> pronoun(v, instr, "ME")
    "раньше" -> l(comeScalarly("verb" to v0.lv, "order" to "EARLIER"))
    "случай" -> noun(v, nom, "THING")
    "случился" -> finiteVerb(v, "PAST") + l(sInstr("head" to v0, "noun" to v[sInstr].lv)) + sem(v0, "type" to "HAPPEN", "experiencer" to v[sInstr])
    "со" -> l(sInstr("noun" to v0), instr("noun" to v0.lv, "head" to Any()))
    "удивительный" -> l(nom("noun" to v0.lv), sem(v0, "property", "AMAZING"))
    "что" -> pronoun(v, nom, "wh")
    "я" -> pronoun(v, nom, "ME")
    ":" -> l(verb("verb" to v0.lv, "last" to true), verb("verb" to v[1].lv, "first" to true), sem(v0, "elaboration", v[1]))
    else -> l()
  }
}

fun l(vararg mites: Mite) = listOf(*mites)
fun noun(v: Vars, case: Construction, typ: String) = l(case("noun" to v[0]), sem.t(v[0], typ))
fun pronoun(v: Vars, case: Construction, typ: String) = noun(v, case, typ)
fun finiteVerb(v: Vars, time: String) = l(verb("verb" to v[0]), nom("head" to v[0], "noun" to v[nom].lv)) + sem(v[0], "time" to time, "arg1" to v[nom])
