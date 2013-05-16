package cons4.constructions

import cons4.Construction
import cons4.Variable
import cons4.Mite

object word: Construction()
object sem: Construction() {
  fun invoke(frame: Variable, attr: String, value: Any) = invoke("frame" to frame, "attr" to attr, "value" to value)
  fun t(frame: Variable, value: String) = invoke(frame, "type", value)

  fun invoke(frame: Variable, vararg args: Pair<String, Any>): List<Mite> = args.map { invoke(frame, it.first, it.second) }
}

object nom: Construction()
object instr: Construction()

object sInstr: Construction()

object verb: Construction()
object elaboration: Construction()

fun happy(mite: Mite): Boolean {
  return when(mite.cxt) {
    nom, instr, sInstr -> mite["noun"] != null && mite["head"] != null
    else -> true
  }
}