package cons4

import cons4.constructions.sem
import java.util.ArrayList

public class Chart(activeMites: Collection<Mite>) {
  val assignments : List<Assignment>
  {
    val assignments = ArrayList<Assignment>()
    for (mite in activeMites) {
      if (mite.cxt == sem) {
        assignments.add(Assignment(mite["frame"] as Variable, mite["attr"] as String, mite["value"]!!))
      }
    }
    this.assignments = assignments
  }

  fun presentable(): String = assignments.map { it.toString() }.toString() + "\n"

}

data class Assignment(
        val frame: Variable,
        val property: String,
        val value: Any) {

  fun toString(): String = "${frame}.$property=${value}"

}
