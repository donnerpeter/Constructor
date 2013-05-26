package cons4.enrichment

import cons4.constructions.*
import cons4.Mite
import cons4.Vars
import cons4.Construction
import cons4.Util
import java.util.ArrayList

fun handleWord(w: String): List<Mite> {
  val v = Vars()
  val v0 = v[0]

  if (Util.parseNumber(w) != null) {
    return noun(v, nom, w) + sem(v0, "number" to "true")
  }

  return when (w) {
    "7", "8" -> noun(v, nom, w)
    "вдруг" -> l(verb("verb" to v0.lv)) + sem(v0, "manner" to "SUDDENLY")
    "забыл" -> finiteVerb(v, "PAST", agrGender="m", agrNumber="sg") + sem.t(v0, "FORGET") + arg(v, comp, "arg2", "comp")
    "и" -> l(seq("conj" to "and", "seqVar" to v0, "left" to v[1], "right" to v[2]))
    "идет" -> finiteVerb(v, "PRESENT", agrGender="m", agrNumber="sg", agrPerson=3) + l(comeScalarly("head" to v0))
    "или" -> l(seq("conj" to "or", "seqVar" to v0, "left" to v[1], "right" to v[2]))
    "их" -> pronoun(v, acc, "THEY")
    "к" -> preposition(v, kDat, dat)
    "мной" -> pronoun(v, instr, "ME")
    "отправился" -> finiteVerb(v, "PAST", agrGender="m", agrNumber="sg") + sem.t(v0, "GO_OFF") + arg(v, kDat, "goal")
    "раньше" -> l(comeScalarly("head" to v0.lv, "order" to "EARLIER"))
    "случай" -> noun(v, nom, "THING")
    "случился" -> finiteVerb(v, "PAST", agrGender="m", agrNumber="sg") + sem.t(v0, "HAPPEN") + arg(v, sInstr, "experiencer")
    "со" -> preposition(v, sInstr, instr)
    "соседям" -> noun(v, dat, "NEIGHBOURS")
    "спросил" -> finiteVerb(v, "PAST", agrGender="m", agrNumber="sg") + sem.t(v0, "ASK") + accArg(v)
    "удивительный" -> l(nom("noun" to v0.lv), sem(v0, "property", "AMAZING"))
    "что" -> pronoun(v, nom, "wh") + l(comp("comp" to v[2]), question("head" to v[2], "first" to true), questionVariants("wh" to v0))
    "я" -> pronoun(v, nom, "ME")
    "," -> l(comp(), semSectionEnd("id" to v0))
    ":" -> l(semSectionEnd("id" to v0), verb("verb" to v0.lv, "last" to true), elaboration("head" to v0, "elaboration" to v[1].lv, "first" to true), sem(v0, "elaboration", v[1]))
    "-" -> l(questionVariants("variants" to v0, "dummyHead" to v[1]), semSectionEnd("id" to v0))
    else -> l()
  }
}

fun l(vararg mites: Mite) = listOf(*mites)
fun preposition(v: Vars, prepCxt: Construction, nounCxt: Construction) = l(prepCxt("noun" to v[0]), nounCxt("noun" to v[0].lv, "head" to v[1]))
fun noun(v: Vars, case: Construction, typ: String) = l(case("noun" to v[0]), sem.t(v[0], typ))
fun pronoun(v: Vars, case: Construction, typ: String) = noun(v, case, typ)
fun arg(v: Vars, argCxt: Construction, relation: String, childAttr: String = "noun") = l(argCxt("head" to v[0], childAttr to v[argCxt].lv), sem(v[0], relation, v[argCxt]))
fun accArg(v: Vars) = arg(v, acc, "arg2")

fun finiteVerb(v: Vars, time: String, agrGender: String? = null, agrNumber: String? = null, agrPerson: Int? = null): List<Mite> {
  val nomArgs : ArrayList<Pair<String, Any>> = arrayListOf("head" to v[0], "noun" to v[nom].lv)
  if (agrGender != null) nomArgs.add("agrGender" to agrGender!!)
  if (agrNumber != null) nomArgs.add("agrNumber" to agrNumber!!)
  if (agrPerson != null) nomArgs.add("agrPerson" to agrPerson!!)
  return l(verb("verb" to v[0]), nom(nomArgs)) + sem(v[0], "time" to time, "arg1" to v[nom])
}

