package cons4

public class Chart(val assignments: List<Assignment>) {

  fun presentable(): String = assignments.map { it.toString() }.toString() + "\n"

}

data class Assignment(
        val frame: Variable,
        val property: String,
        val value: Any) {

  fun toString(): String = "${frame}.$property=${value}"

}
