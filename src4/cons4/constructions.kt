package cons4.constructions

import cons4.Construction
import cons4.Variable
import cons4.Mite
import cons4.ParsingState
import java.util.ArrayList
import cons4.enrichment.handleWord

object word: Construction()
object sem: Construction() {
  fun invoke(frame: Variable, attr: String, value: Any) = invoke("frame" to frame, "attr" to attr, "value" to value)
  fun t(frame: Variable, value: String) = invoke(frame, "type", value)

  fun invoke(frame: Variable, vararg args: Pair<String, Any?>): List<Mite> = args.map { invoke(frame, it.first, it.second!!) }
}

object nom: Construction()
object instr: Construction()

object sInstr: Construction()

object comp: Construction()
object question: Construction()

object verb: Construction()
object elaboration: Construction()

object comeScalarly: Construction()

fun happy(mite: Mite): Boolean {
  return when(mite.cxt) {
    nom, instr, sInstr -> mite.has("noun", "head")
    verb -> mite.hasHard("verb")
    comeScalarly -> mite.has("verb", "order")
    comp -> mite.hasHard("head", "comp")
    else -> true
  }
}

fun showMoreMites(mite: Mite, state: ParsingState): Collection<Mite> {
  if ((mite.cxt == instr || mite.cxt == sInstr) && mite["head"] != null) {
    val head = mite.primaries.find { it["head"] != null }!!
    val headIndex = state.getAtomIndex(head)
    return state.mites[headIndex]
  }

  return listOf()
}

fun canUnify(left: Mite, right: Mite): Boolean {
  if (left.cxt == verb && (left["last"] == true || right["first"] == true)) return false
  return true
}

fun enrich(mite: Mite): List<Mite> {
  if (mite.cxt == comeScalarly && mite.has("verb", "order")) {
    return sem(mite.v("verb"), "type" to "COME_SCALARLY", "order" to mite["order"])
  }
  if (mite.cxt == question && mite.has("frame", "content")) return sem(mite.v("frame"), "type" to "question", "content" to mite["content"])

  return when (mite.cxt) {
    word -> handleWord(mite["word"] as String)
    else -> ArrayList()
  }
}
