package cons4

import cons4.constructions.*
import java.util.ArrayList
import java.util.HashMap
import java.util.LinkedHashSet

object Util {
  fun parseNumber(word: String?): Int? {
    if (word == null || word.length() == 0 || !Character.isDigit(word.charAt(0))) {
      return null
    }

    try {
      return Integer.parseInt(word)
    } catch (ignore: NumberFormatException) {
      return null
    }
  }

}

fun l(vararg mites: Mite) = listOf(*mites)

fun preposition(v: Vars, prepCxt: Construction, nounCxt: Construction) = l(prepCxt("noun" to v[0]), nounCxt("noun" to v[0].lv, "head" to v[1]))

fun noun(v: Vars, case: Construction, typ: String) =
        pronoun(v, case, typ) + l(possessive("head" to v[0], "possessor" to v[possessive].lv), sem(v[0], "arg1", v[possessive])).optional()

fun pronoun(v: Vars, case: Construction, typ: String, vararg additional: Pair<String, Any>): List<Mite> {
  val caseArgs : ArrayList<Pair<String, Any>> = arrayListOf("noun" to v[0])
  caseArgs.addAll(additional.toList())
  return l(case(caseArgs), sem.t(v[0], typ))
}

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

