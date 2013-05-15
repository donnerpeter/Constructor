package cons4

import cons4.constructions.sem
import java.util.ArrayList
import java.util.HashMap
import java.util.LinkedHashSet
import java.util.LinkedHashMap

private fun createFrames(activeMites: Collection<Mite>, chart: Chart): Map<Variable, Frame> {
  val allVars = LinkedHashSet<Variable>()
  for (mite in activeMites) {
    for (value in mite.args.values()) {
      if (value is Variable) {
        allVars.add(value.base)
      }
    }
  }

  val unifications = HashMap<Variable, LinkedHashSet<Variable>>()
  for (v in allVars) {
    val primaries = v.primaries
    val existingKey = primaries.find { unifications.containsKey(it) }
    val group: LinkedHashSet<Variable> = if (existingKey != null) unifications[existingKey]!! else LinkedHashSet()
    group.addAll(primaries)
    unifications[v] = group
    primaries.forEach { unifications[it] = group }
  }

  val frames = LinkedHashMap<Variable, Frame>()
  for (v in allVars) {
    val base = unifications[v]!!.toList()[0]
    val frame = frames[base] ?: Frame(chart, base)
    frames[base] = frame
    frames[v] = frame
  }
  return frames
}

public class Chart(activeMites: Collection<Mite>) {
  val var2Frames: Map<Variable, Frame> = createFrames(activeMites, this)
  val assignments : List<Assignment>
  {
    val assignments = ArrayList<Assignment>()
    for (mite in activeMites) {
      if (mite.cxt == sem) {
        val value = mite["value"]
        val convertedValue: Any? = if (value is Variable) var2Frames[value] else value
        assignments.add(Assignment(var2Frames[mite["frame"] as Variable]!!, mite["attr"] as String, convertedValue!!))
      }
    }

    this.assignments = assignments
  }

  fun getFrames() = var2Frames.values().toList()

  fun presentable(): String {
    val frameId = HashMap<Frame, String>()
    var counter = 'A'
    val namer = { (frame: Frame) -> frameId.getOrPut(frame) { "${counter++}" } }

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

  val allAssignments: List<Assignment> get() = chart.assignments.filter { it.frame == this }

  fun allAssignments(attr: String): List<Assignment> = allAssignments.filter { it.property == attr }

  fun definedAttributeValue(attr: String) = allAssignments(attr).firstOrNull()?.value

  fun s(attr: String) = definedAttributeValue(attr) as String?
  fun f(attr: String) = definedAttributeValue(attr) as Frame?

  fun getType() = definedAttributeValue("type")
  fun getTypeInferred() = getType()

  fun usages(attr: String) = chart.assignments.filter { it.value == this && it.property == attr }.map { it.frame }

  fun flatten() = listOf(this)

  fun hasType() = flatten()[0].getType() != null

  fun allMetas(attr: String): List<Frame> = listOf()

  fun findMeta(attr: String, metaType:String) = allMetas(attr).find { it.getType() == metaType }
}