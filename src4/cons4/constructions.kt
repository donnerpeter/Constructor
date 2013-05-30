package cons4.constructions

import cons4.Construction
import cons4.Variable
import cons4.Mite
import cons4.ParsingState
import java.util.ArrayList
import cons4.enrichment.handleWord
import cons4.enrichment.l
import cons4.enrichment.optional
import cons4.enrichment.xor

object emptyCxt: Construction()
object word: Construction()
object semSectionEnd: Construction()
object sem: Construction() {
  fun invoke(frame: Variable, attr: String, value: Any) = invoke("frame" to frame, "attr" to attr, "value" to value)
  fun t(frame: Variable, value: String) = invoke(frame, "type", value)

  fun invoke(frame: Variable, vararg args: Pair<String, Any?>): List<Mite> = args.map { invoke(frame, it.first, it.second!!) }
}
object phrase: Construction() {
  fun invoke(head: Variable, kind: String) = invoke("head" to head, "kind" to kind)
  fun invoke(head: Variable) = invoke("head" to head)
}

open class CaseConstruction: Construction()

object nom: CaseConstruction()
object gen: CaseConstruction()
object dat: CaseConstruction()
object acc: CaseConstruction()
object instr: CaseConstruction()
object prep: CaseConstruction()

open class PPConstruction: Construction()

object sInstr: PPConstruction()
object kDat: PPConstruction()
object poDat: PPConstruction()

object possessive: Construction()

object seq: Construction()

object comp: Construction()
object conditionComp: Construction()
object question: Construction()
object questionVariants: Construction()
object clauseType: Construction()
object control: Construction()
object complementizer: Construction()

object elaboration: Construction()

object comeScalarly: Construction()

fun happy(mite: Mite): Boolean {
  return when(mite.cxt) {
    is CaseConstruction, is PPConstruction -> mite.hasHard("noun", "head")
    phrase -> mite.hasHard("head")
    comeScalarly -> mite.has("order") && mite.hasHard("head")
    comp, conditionComp -> mite.hasHard("head", "comp")
    possessive -> mite.hasHard("head", "possessor")
    control -> mite.hasHard("head", "slave")
    elaboration -> mite.hasHard("head", "elaboration")
    questionVariants -> mite.has("wh", "variants")
    question, complementizer -> mite.hasHard("head", "content")
    clauseType -> mite.hasHard("clauseParent") && mite.has("hasComma")
    else -> true
  }
}

fun hasHead(mite: Mite): Boolean {
  return mite.hasHard("head")
}

fun isPenetrable(mite: Mite) = !(mite.cxt == phrase && mite["kind"] == "verb")

fun canUnify(left: Mite, right: Mite): Boolean {
  if (left.cxt == emptyCxt || left.cxt == sem) return false
  if (left["last"] == true || right["first"] == true) return false
  return true
}

fun enrich(state: ParsingState, mite: Mite): List<Mite> {
  if (mite.cxt == comeScalarly && mite.has("head", "order")) {
    return sem(mite.v("head"), "type" to "COME_SCALARLY", "order" to mite["order"])
  }
  if (mite.cxt == question && happy(mite)) {
    return sem(mite.v("head"), "type" to "question", "content" to mite["content"]) + l(questionVariants("wh" to mite.v("questioned"))).optional()
  }
  if (mite.cxt == complementizer && happy(mite)) return sem(mite.v("head"), "type" to "fact", "content" to mite["content"])

  if (mite.cxt == questionVariants && mite.has("wh", "variants")) {
    return sem(mite.v("wh"), "variants" to mite["variants"]) + listOf(nom("head" to mite.v("dummyHead"), "noun" to mite.v("variants").lv))
  }
  if (mite.cxt == seq && mite.atom && mite.has("conj")) {
    val seqVar = mite.v("seqVar")
    val left = mite.v("left")
    val right = mite.v("right")

    val visibleMites = state.getVisibleMites(state.getAtomIndex(mite), true)

    val result = ArrayList<Mite>()
    for (visible in visibleMites) {
      if (visible.cxt == nom && visible.has("noun") && !visible.has("head")) {
        result.add(nom("noun" to seqVar))
        result.addAll(l(nom("head" to seqVar, "noun" to left.lv, "last" to true), nom("head" to seqVar, "noun" to right.lv, "first" to true)))
        result.addAll(l(sem(seqVar, "conj", mite["conj"]!!), sem(seqVar, "member", left), sem(seqVar, "member", right)))
      }
      else if (visible.cxt == possessive && visible.has("possessor") && !visible.has("head")) {
        result.add(possessive("possessor" to seqVar))
        result.addAll(l(possessive("head" to seqVar, "possessor" to left.lv, "last" to true), possessive("head" to seqVar, "possessor" to right.lv, "first" to true)))
        result.addAll(l(sem(seqVar, "conj", mite["conj"]!!), sem(seqVar, "member", left), sem(seqVar, "member", right)))
      }
      else if (visible.cxt == phrase && visible.hasHard("head") && visible["kind"] == "verb") {
        val nomArg = visibleMites.find { it.atom && it.cxt == nom && it.hasHard("head") && it.has("noun") && !it.hasHard("noun") && it.v("head") == visible.v("head") }
        if (nomArg != null) {
          result.addAll(l(nom("head" to right.lv, "noun" to nomArg.v("noun").base, "first" to true)))
        }
        result.addAll(l(phrase("head" to left.lv, "kind" to "verb", "last" to true), phrase("head" to right.lv, "kind" to "verb", "first" to true)))
        result.addAll(l(sem(seqVar, "conj", mite["conj"]!!), sem(seqVar, "member", left), sem(seqVar, "member", right)))
      }
    }
    return result
  }
  if (mite.cxt == phrase && mite.hasHard("head") && mite["kind"] == "verb") {
    val head = mite.primaries.find { it.hasHard("head") }!!.v("head")
    return l(elaboration("elaboration" to head)).optional() +
           l(question("content" to head)).xor(l(complementizer("content" to head))).optional()
  }
  if (mite.cxt == conditionComp && happy(mite)) {
    return sem(mite.v("head"), "whenCondition" to mite.v("comp"))
  }

  return when (mite.cxt) {
    word -> handleWord(mite["word"] as String)
    else -> ArrayList()
  }
}
