package cons4

import cons4.constructions.*
import java.util.ArrayList
import java.util.HashMap
import java.util.LinkedHashSet
import java.util.HashSet

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

private fun allCombinations(updates: List<List<Mite>>): Set<Set<Mite>> {
  if (updates.empty) return LinkedHashSet()
  if (updates.size == 1) return LinkedHashSet(updates[0].map { setOf(it) })

  val next = allCombinations(updates.subList(1, updates.size))
  val result = LinkedHashSet<Set<Mite>>()
  val commonMites = HashSet<Mite>()
  for (mite in updates[0]) {
    val existingGroup = next.find { mite in it }
    if (existingGroup != null) {
      result.add(existingGroup)
      commonMites.add(mite)
    }
  }
  for (mite in updates[0].filter { it !in commonMites }) {
    if (mite in commonMites) continue
    for (rest in next.filter { it !in result }) {
      val merged = LinkedHashSet<Mite>()
      merged.add(mite)
      merged.addAll(rest)
      result.add(merged)
    }
  }
  return result
}

fun multiXor(updates: List<List<Mite>>): List<Mite> {
  var id = 'a'
  val t = Tokens(updates)
  val map = HashMap<Mite, LinkedHashSet<Token>>()
  for (combo in allCombinations(updates)) {
    if (combo.size == 1) continue

    val token = t["${id}"]
    id++
    for (mite in combo) {
      map.getOrPut(mite) { LinkedHashSet<Token>() }.add(token)
    }
  }
  val processed = HashSet<Mite>()
  val result = ArrayList<Mite>()
  for (update in updates) {
    for (mite in update) {
      if (processed.add(mite)) {
        val xors = map[mite]
        if (xors == null) {
          result.add(mite)
        } else {
          val mockMite = mite.unify(mite.cxt("xor" to xors!!))!!
          result.add(mite.copy(args=mockMite.args))
        }
      }
    }
  }
  return result
}

fun List<Mite>.xor(list2: List<Mite>): List<Mite> = multiXor(listOf(this, list2))