package cons4.constructions

import cons4.Construction
import cons4.Variable
import cons4.Mite
import cons4.ParsingState
import java.util.ArrayList
import cons4.enrichment.handleWord
import cons4.l
import cons4.optional
import cons4.xor
import cons4.multiXor

object emptyCxt: Construction()
object word: Construction()
object semSectionEnd: Construction()
object sem: Construction() {
  fun invoke(frame: Variable, attr: String, value: Any) = invoke("frame" to frame, "attr" to attr, "value" to value)
  fun t(frame: Variable, value: String) = invoke(frame, "type", value)

  fun invoke(frame: Variable, vararg args: Pair<String, Any?>): List<Mite> = args.map { invoke(frame, it.first, it.second!!) }
}
object phrase: Construction("*head") {
  fun invoke(head: Variable, kind: String) = invoke("head" to head, "kind" to kind)
  fun invoke(head: Variable) = invoke("head" to head)
}

open class CaseConstruction: Construction("*noun", "*head")

object nom: CaseConstruction()
object gen: CaseConstruction()
object dat: CaseConstruction()
object acc: CaseConstruction()
object instr: CaseConstruction()
object prep: CaseConstruction()

open class PPConstruction: Construction("*noun", "*head")

object sInstr: PPConstruction()
object kDat: PPConstruction()
object poDat: PPConstruction()

object possessive: Construction("*head", "*possessor")

object seq: Construction()
object mergedMite: Construction()
object seqContinuation: Construction("seqVar", "active")

object comp: Construction("*head", "*comp")
object conditionComp: Construction("*head", "*comp")
object question: Construction("*head", "*content")
object questionVariants: Construction("wh", "variants")
object clauseType: Construction("*clauseParent", "*head")
object sentence: Construction("*head", "*verb")
object control: Construction("*head", "*slave")
object complementizer: Construction("*head", "*content")

object elaboration: Construction("*head", "*elaboration")
object contrastiveTopic: Construction("head", "active")

object comeScalarly: Construction("order", "*head")

fun hasHead(mite: Mite): Boolean {
  return mite.hasHard("head")
}

fun isPenetrable(mite: Mite) = !(mite.cxt == phrase && mite["kind"] == "verb")

fun canUnify(left: Mite, right: Mite): Boolean {
  if (left.cxt == emptyCxt || left.cxt == sem || left.cxt == semSectionEnd) return false
  if (left["last"] == true || right["first"] == true) return false
  return true
}

fun enrich(state: ParsingState, mite: Mite): List<Mite> {
  if (mite.cxt == comeScalarly && mite.happy) {
    return sem(mite.v("head"), "type" to "COME_SCALARLY", "order" to mite["order"])
  }
  if (mite.cxt == question && mite.happy) {
    return sem(mite.v("head"), "type" to "question", "content" to mite["content"]) + l(questionVariants("wh" to mite.v("questioned"))).optional()
  }
  if (mite.cxt == complementizer && mite.happy) return sem(mite.v("head"), "type" to "fact", "content" to mite["content"])

  if (mite.cxt == questionVariants && mite.has("wh", "variants")) {
    return sem(mite.v("wh"), "variants" to mite["variants"]) + listOf(nom("head" to mite.v("dummyHead"), "noun" to mite.v("variants").lv))
  }
  if (mite.cxt == seq && mite.atom && mite.has("conj")) {
    val visibleMites = state.getVisibleMites(state.getAtomIndex(mite), true)

    val result = ArrayList<List<Mite>>()
    for (visible in visibleMites) {
      if (visible.cxt is CaseConstruction && visible.has("noun") && !visible.has("head") ||
          visible.cxt == possessive && visible.has("possessor") && !visible.has("head") ||
          visible.cxt == phrase && visible.hasHard("head") && visible["kind"] == "verb") {
        result.add(l(mergedMite("mergedMite" to visible, "seqMite" to mite)))
      }
    }
    return multiXor(result)
  }
  if (mite.cxt == mergedMite) {
    val merged = mite["mergedMite"] as Mite
    val seqMite = mite["seqMite"] as Mite
    val seqVar = seqMite.v("seqVar")
    val left = seqMite.v("left")
    val right = seqMite.v("right")

    val visibleMites = state.getVisibleMites(state.getAtomIndex(mite), true)
    val result = ArrayList<Mite>()
    if (merged.cxt is CaseConstruction) {
      result.add(merged.cxt("noun" to seqVar))
      result.addAll(l(merged.cxt("head" to seqVar, "noun" to left.lv, "last" to true), merged.cxt("head" to seqVar, "noun" to right.lv, "first" to true)))
    } else if (merged.cxt == possessive) {
      result.add(possessive("possessor" to seqVar))
      result.addAll(l(possessive("head" to seqVar, "possessor" to left.lv, "last" to true), possessive("head" to seqVar, "possessor" to right.lv, "first" to true)))
    } else if (merged.cxt == phrase) {
      val nomArg = visibleMites.find { it.atom && it.cxt == nom && it.hasHard("head") && it.has("noun") && !it.hasHard("noun") && it.v("head") == merged.v("head") }
      if (nomArg != null) {
        result.addAll(l(nom("head" to right.lv, "noun" to nomArg.v("noun").base, "first" to true)))
      }
      result.addAll(l(phrase("head" to left.lv, "kind" to "verb", "last" to true), phrase("head" to right.lv, "kind" to "verb", "first" to true)))
    }

    result.addAll(l(sem(seqVar, "member", left))/*.xor(l(seqContinuation("active" to true, "seqVar" to seqVar.lv, "last" to true)))*/ + l(sem(seqVar, "member", right)))
    val conj = seqMite["conj"] as String
    if (conj != ",") {
      result.add(sem(seqVar, "conj", conj))
    }
    result.add(sem(seqVar, "member", right))
//    result.addAll(seqContinuation("seqVar" to seqVar, "first" to true).optional())
    return result
  }
  if (mite.cxt == phrase && mite.hasHard("head")) {
    val head = mite.primaries.find { it.hasHard("head") }!!.v("head")
    val result = ArrayList(contrastiveTopic("head" to head, "first" to true).optional())
    if (mite["kind"] == "verb") {
      result.addAll(multiXor(listOf(l(elaboration("elaboration" to head)),
              l(question("content" to head)),
              l(complementizer("content" to head)),
              l(sentence("verb" to head, "first" to true)))))
    }
    return result
  }
  if (mite.cxt == conditionComp && mite.happy) {
    return sem(mite.v("head"), "whenCondition" to mite.v("comp"))
  }

  return when (mite.cxt) {
    word -> handleWord(mite["word"] as String)
    else -> ArrayList()
  }
}
