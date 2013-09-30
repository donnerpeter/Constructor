package cons4

class Frame(val chart: Chart, val variable: Variable) {
  fun toString() = variable.toString()

  val allAssignments: List<Assignment> get() = chart.assignments.filter { it.frame == this }

  fun allAssignments(attr: String): List<Assignment> = allAssignments.filter { it.property == attr }

  fun definedAttributeValue(attr: String) = allAssignments(attr).firstOrNull()?.value

  private fun findController(): Frame? {
    val group = usages("member").firstOrNull() ?: this
    val clause = group.usages("content").firstOrNull()
    if (clause == null || clause.getType() != "fact" && clause.getType() != "question") {
      return null
    }

    val controller = clause.findClauseController()
    assert(controller != this)
    return controller
  }

  fun findClauseController(): Frame? {
    val group = usages("member").firstOrNull() ?: this
    var controller = group.usages("arg2").firstOrNull()
    controller = controller ?: group.usages("theme").firstOrNull()
    controller = controller ?: group.usages("message").firstOrNull()
    controller = controller ?: group.usages("question").firstOrNull()
    return controller
  }


  fun f(attr: String): Frame? {
    val defined = definedAttributeValue(attr)
    if (defined is Frame) return defined

    if (attr == "arg1") {
      if (getType() == "NEIGHBOURS") {
        val verb = usages("arg2").firstOrNull() ?: usages("goal").firstOrNull()
        return verb?.f("arg1")
      }

    }

    return null
  }

  fun s(attr: String): String? {
    val defined = definedAttributeValue(attr)
    if (defined is String) return defined

    if (attr == "given") {
      when (getType()) {
        "THING", "HAMMER", "BENCH", "FINGER", "JAW", "WATER_MELON" -> return "false"
        else -> return "true"
      }
    }

    return null
  }

  fun getType() = definedAttributeValue("type")
  fun getTypeInferred() = getType()

  fun resolve(): Frame {
    if (!hasType()) {
      val master = usages("arg1").firstOrNull()
      if (master?.getType() == "JAW") {
        val verb = chart.getFrames().find { it.definedAttributeValue("arg1") != null }
        if (verb != null) {
          return verb.definedAttributeValue("arg1") as Frame
        }
      }
      val controller = master?.findController()
      val cArg1 = controller?.f("arg1")
      if (cArg1 != null) {
        return cArg1
      }

    }
    return this
  }

  fun usages(attr: String) = chart.assignments.filter { it.value == this && it.property == attr }.map { it.frame }

  fun flatten(): List<Frame> {
    val members = allAssignments("member")
    if (members.notEmpty) return members.map { it.value as Frame }

    return listOf(this)
  }

  fun hasType() = flatten()[0].getType() != null

  fun allMetas(attr: String): List<Frame> = listOf()

  fun findMeta(attr: String, metaType:String) = allMetas(attr).find { it.getType() == metaType }
}