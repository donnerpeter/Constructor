package cons4.constructions

import cons4.Construction
import cons4.Variable
import cons4.Mite
import cons4.ParsingState
import java.util.ArrayList
import cons4.enrichment.handleWord
import cons4.enrichment.l

object word: Construction()
object semSectionEnd: Construction()
object sem: Construction() {
  fun invoke(frame: Variable, attr: String, value: Any) = invoke("frame" to frame, "attr" to attr, "value" to value)
  fun t(frame: Variable, value: String) = invoke(frame, "type", value)

  fun invoke(frame: Variable, vararg args: Pair<String, Any?>): List<Mite> = args.map { invoke(frame, it.first, it.second!!) }
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

object seq: Construction()

object comp: Construction()
object question: Construction()
object questionVariants: Construction()

object verb: Construction()
object elaboration: Construction()

object comeScalarly: Construction()

fun happy(mite: Mite): Boolean {
  return when(mite.cxt) {
    is CaseConstruction, is PPConstruction -> mite.hasHard("noun", "head")
    verb -> mite.hasHard("verb")
    comeScalarly -> mite.has("head", "order")
    comp -> mite.hasHard("head", "comp")
    elaboration -> mite.hasHard("head", "elaboration")
    questionVariants -> mite.has("wh", "variants")
    question -> mite.hasHard("head", "content")
    else -> true
  }
}

fun hasHead(mite: Mite): Boolean {
  if (mite.cxt == verb) return mite.hasHard("verb")
  return mite.hasHard("head")
}

fun canUnify(left: Mite, right: Mite): Boolean {
  if (left["last"] == true || right["first"] == true) return false
  return true
}

fun enrich(state: ParsingState, mite: Mite): List<Mite> {
  if (mite.cxt == comeScalarly && mite.has("head", "order")) {
    return sem(mite.v("head"), "type" to "COME_SCALARLY", "order" to mite["order"])
  }
  if (mite.cxt == question && mite.has("head", "content")) return sem(mite.v("head"), "type" to "question", "content" to mite["content"])
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
      } else if (visible.cxt == verb && visible.hasHard("verb")) {
        val nomArg = visibleMites.find { it.atom && it.cxt == nom && it.hasHard("head") && it.has("noun") && !it.hasHard("noun") && it.v("head") == visible.v("verb") }
        if (nomArg != null) {
          result.addAll(l(nom("head" to right.lv, "noun" to nomArg.v("noun").base, "first" to true)))
        }
        result.addAll(l(verb("verb" to left.lv, "last" to true), verb("verb" to right.lv, "first" to true)))
        result.addAll(l(sem(seqVar, "conj", mite["conj"]!!), sem(seqVar, "member", left), sem(seqVar, "member", right)))
      }
    }
    return result
  }
  if (mite.cxt == verb && mite.atom && mite.hasHard("verb")) {
    return l(elaboration("elaboration" to mite.v("verb")), question("content" to mite.v("verb")))
  }

  return when (mite.cxt) {
    word -> handleWord(mite["word"] as String)
    else -> ArrayList()
  }
}
