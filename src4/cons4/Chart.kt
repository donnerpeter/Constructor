package cons4

import cons4.constructions.sem
import java.util.ArrayList
import java.util.HashMap

public class Chart(activeMites: Collection<Mite>) {
  val assignments : List<Assignment>
  val frames : Map<Variable, Frame>
  {
    val assignments = ArrayList<Assignment>()
    val frames = HashMap<Variable, Frame>()
    for (mite in activeMites) {
      for (value in mite.args.values()) {
        if (value is Variable) {
          frames.put(value, Frame(this, value))
        }
      }

      if (mite.cxt == sem) {
        assignments.add(Assignment(frames[mite["frame"] as Variable]!!, mite["attr"] as String, mite["value"]!!))
      }
    }

    this.assignments = assignments
    this.frames = frames
  }

  fun presentable(): String = assignments.map { it.toString() }.toString() + "\n"

}

data class Assignment(
        val frame: Frame,
        val property: String,
        val value: Any) {

  fun toString(): String = "${frame}.$property=${value}"

}

class Frame(val chart: Chart, val variable: Variable) {
  fun toString() = variable.toString()
}