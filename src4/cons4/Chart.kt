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
        val value = mite["value"]
        val convertedValue: Any? = if (value is Variable) frames[value] else value
        assignments.add(Assignment(frames[mite["frame"] as Variable]!!, mite["attr"] as String, convertedValue!!))
      }
    }

    this.assignments = assignments
    this.frames = frames
  }

  fun presentable(): String {
    val frameId = HashMap<Frame, String>()
    var counter = 'A'
    val namer = { (frame: Frame) ->
      var id = frameId[frame]
      if (id == null) {
        id = "${counter++}"
        frameId[frame] = id!!
      }
      id
    }

    var result = ""
    for (a in assignments) {
      result += namer(a.frame) + ".${a.property}="
      result += if (a.value is Frame) namer(a.value) else a.value as String
      result += "\n"
    }
    return result
  }

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