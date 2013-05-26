package cons4.enrichment

import cons4.constructions.*
import cons4.Mite
import cons4.Vars
import cons4.Construction
import cons4.Util
import java.util.ArrayList
import cons4.Token
import java.util.HashMap
import java.util.LinkedHashSet
import cons4.Tokens

fun handleWord(w: String): List<Mite> {
  val v = Vars()
  val v0 = v[0]

  if (Util.parseNumber(w) != null) {
    return noun(v, nom, w) + sem(v0, "number" to "true")
  }

  return when (w) {
    "было" -> l(phrase(v0, "verb"), nom("head" to v0, "noun" to v[1].lv)) + sem(v0, "time" to "PAST")
    "вдруг" -> l(phrase(v0.lv, "verb")) + sem(v0, "manner" to "SUDDENLY")
    "думают" -> finiteVerb(v, "PRESENT", "THINK", agrNumber="pl", agrPerson=3) + arg(v, poDat, "topic") + accArg(v)
    "же" -> l(phrase("head" to v0.lv, "last" to true))
    "забыл" -> finiteVerb(v, "PAST", "FORGET", agrGender="m", agrNumber="sg") + arg(v, comp, "arg2", "comp")
    "и" -> l(seq("conj" to "and", "seqVar" to v0, "left" to v[1], "right" to v[2]))
    "идет" -> finiteVerb(v, "PRESENT", agrGender="m", agrNumber="sg", agrPerson=3) + l(comeScalarly("head" to v0))
    "или" -> l(seq("conj" to "or", "seqVar" to v0, "left" to v[1], "right" to v[2]))
    "их" -> pronoun(v, acc, "THEY").xor(l(possessive("possessor" to v0), sem.t(v0, "THEY")))
    "к" -> preposition(v, kDat, dat)
    "каково" -> sem(v0, "type" to "degree", "arg1" to v[1], "arg2" to v[2]) + sem.t(v[2], "wh") + l(nom("head" to v0.lv, "noun" to v[1].lv), phrase(v0.lv, "verb"), phrase(v[2], "shortAdj"))
    "когда" -> l(conditionComp("comp" to v0), phrase(v0.lv, "verb"))
    "мной" -> pronoun(v, instr, "ME")
    "мое" -> l(possessive("possessor" to v0), sem.t(v0, "ME"))
    "обнаружили" -> finiteVerb(v, "PAST", "DISCOVER", agrNumber="pl")
    "они" -> pronoun(v, nom, "THEY")
    "отправился" -> finiteVerb(v, "PAST", "GO_OFF", agrGender="m", agrNumber="sg") + arg(v, kDat, "goal")
    "по" -> preposition(v, poDat, dat)
    "поводу" -> noun(v, dat, "MATTER")
    "раньше" -> l(comeScalarly("head" to v0.lv, "order" to "EARLIER"))
    "случай" -> noun(v, nom, "THING")
    "случился" -> finiteVerb(v, "PAST", "HAPPEN", agrGender="m", agrNumber="sg") + arg(v, sInstr, "experiencer")
    "со" -> preposition(v, sInstr, instr)
    "соседям" -> noun(v, dat, "NEIGHBOURS")
    "спросил" -> finiteVerb(v, "PAST", "ASK", agrGender="m", agrNumber="sg") + accArg(v) + arg(v, comp, "question", "comp")
    "удивительный" -> adj(v, nom, "property", "AMAZING")
    "удивление" -> noun(v, nom, "AMAZE")
    "что" -> pronoun(v, nom, "wh").xor(pronoun(v, acc, "wh")) + l(comp("comp" to v[2]), question("head" to v[2], "first" to true), questionVariants("wh" to v0))
    "этому" -> adj(v, dat, "determiner", "THIS")
    "я" -> pronoun(v, nom, "ME")
    "," -> l(phrase("head" to v0.lv, "kind" to "verb", "last" to true)) + l(comp()).xor(l(conditionComp("head" to v0))) + l(semSectionEnd("id" to v0))
    ":" -> l(semSectionEnd("id" to v0), phrase("head" to v0.lv, "kind" to "verb", "last" to true), elaboration("head" to v0, "elaboration" to v[1].lv, "first" to true), sem(v0, "elaboration", v[1]))
    "-" -> l(questionVariants("variants" to v0, "dummyHead" to v[1]), semSectionEnd("id" to v0))
    else -> l()
  }
}

fun l(vararg mites: Mite) = listOf(*mites)

fun preposition(v: Vars, prepCxt: Construction, nounCxt: Construction) = l(prepCxt("noun" to v[0]), nounCxt("noun" to v[0].lv, "head" to v[1]))

fun noun(v: Vars, case: Construction, typ: String) =
        pronoun(v, case, typ) + l(possessive("head" to v[0], "possessor" to v[possessive].lv), sem(v[0], "arg1", v[possessive])).optional()

fun pronoun(v: Vars, case: Construction, typ: String) = l(case("noun" to v[0]), sem.t(v[0], typ))

fun arg(v: Vars, argCxt: Construction, relation: String, childAttr: String = "noun") =
        l(argCxt("head" to v[0], childAttr to v[argCxt].lv), sem(v[0], relation, v[argCxt]))

fun accArg(v: Vars) = arg(v, acc, "arg2")

fun adj(v: Vars, case: Construction, rel: String, value: String) = l(case("noun" to v[0].lv), sem(v[0], rel, value))

fun finiteVerb(v: Vars, time: String, typ: String? = null, agrGender: String? = null, agrNumber: String? = null, agrPerson: Int? = null): List<Mite> {
  val nomArgs : ArrayList<Pair<String, Any>> = arrayListOf("head" to v[0], "noun" to v[nom].lv)
  if (agrGender != null) nomArgs.add("agrGender" to agrGender!!)
  if (agrNumber != null) nomArgs.add("agrNumber" to agrNumber!!)
  if (agrPerson != null) nomArgs.add("agrPerson" to agrPerson!!)
  val result = arrayListOf(phrase(v[0], "verb"), nom(nomArgs))
  if (typ != null) result.add(sem.t(v[0], typ))
  return result + sem(v[0], "time" to time, "arg1" to v[nom])
}

fun List<Mite>.optional(): List<Mite> = xor(l(emptyCxt()))

fun List<Mite>.xor(list2: List<Mite>): List<Mite> {
  val list1 = this
  val common = list1.filter { it in list2 }
  val map = HashMap<Mite, LinkedHashSet<Token>>()
  var id = 'a'
  val t = Tokens(this to list2)
  for (m1 in list1.filter { it !in common }) {
    for (m2 in list2.filter { it !in common}) {
      val token = t["${id}"]
      id++
      map.getOrPut(m1) { LinkedHashSet<Token>() }.add(token)
      map.getOrPut(m2) { LinkedHashSet<Token>() }.add(token)
    }
  }
  val result = ArrayList<Mite>()
  for (mite in list1 + list2.filter { it !in common}) {
    if (mite in common) {
      result.add(mite)
    } else {
      val mockMite = mite.unify(mite.cxt("xor" to map[mite]!!))!!
      result.add(mite.copy(args=mockMite.args))
    }
  }
  return result
}