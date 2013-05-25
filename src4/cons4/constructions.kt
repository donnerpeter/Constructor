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

object nom: Construction()
object gen: Construction()
object dat: Construction()
object acc: Construction()
object instr: Construction()
object prep: Construction()

object sInstr: Construction()
object kDat: Construction()

object seq: Construction()

object comp: Construction()
object question: Construction()
object questionVariants: Construction()

object verb: Construction()
object elaboration: Construction()

object comeScalarly: Construction()

fun happy(mite: Mite): Boolean {
  return when(mite.cxt) {
    nom, gen, dat, acc, instr, prep, sInstr, kDat -> mite.hasHard("noun", "head")
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
    val result = ArrayList<Mite>()
    for (visible in state.getVisibleMites(state.getAtomIndex(mite), true)) {
      if (visible.cxt == nom && visible.has("noun") && !visible.has("head")) {
        val seqVar = mite.v("seqVar")
        result.add(nom("head" to seqVar, "noun" to mite.v("left").lv, "last" to true))
        result.add(nom("noun" to seqVar))
        result.add(nom("head" to seqVar, "noun" to mite.v("right").lv, "first" to true))
        result.add(sem(seqVar, "conj", mite["conj"]!!))
        result.add(sem(seqVar, "member", mite.v("left")))
        result.add(sem(seqVar, "member", mite.v("right")))
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
