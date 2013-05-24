package cons4

class Frame(val chart: Chart, val variable: Variable) {
  fun toString() = variable.toString()

  val allAssignments: List<Assignment> get() = chart.assignments.filter { it.frame == this }

  fun allAssignments(attr: String): List<Assignment> = allAssignments.filter { it.property == attr }

  fun definedAttributeValue(attr: String) = allAssignments(attr).firstOrNull()?.value

  fun s(attr: String) = definedAttributeValue(attr) as String?
  fun f(attr: String) = definedAttributeValue(attr) as Frame?

  fun getType() = definedAttributeValue("type")
  fun getTypeInferred() = getType()

  fun usages(attr: String) = chart.assignments.filter { it.value == this && it.property == attr }.map { it.frame }

  fun flatten(): List<Frame> {
    val members = allAssignments("member")
    if (members.notEmpty()) return members.map { it.value as Frame }

    return listOf(this)
  }

  fun hasType() = flatten()[0].getType() != null

  fun allMetas(attr: String): List<Frame> = listOf()

  fun findMeta(attr: String, metaType:String) = allMetas(attr).find { it.getType() == metaType }
}