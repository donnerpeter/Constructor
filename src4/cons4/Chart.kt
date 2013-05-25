package cons4

import cons4.constructions.sem
import java.util.ArrayList
import java.util.HashMap
import java.util.LinkedHashSet
import java.util.LinkedHashMap
import cons4.constructions.semSectionEnd

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
    val group = LinkedHashSet(primaries)
    for (atom in primaries) {
      val existingGroup = unifications[atom]
      if (existingGroup != null) {
        group.addAll(existingGroup)
      }
    }

    unifications[v] = group
    group.forEach { unifications[it] = group }
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
  val frameSections = HashMap<Frame, Int>()
  val assignmentSections = HashMap<Assignment, Int>()
  val assignments = ArrayList<Assignment>();
  {
    var section = 1
    for (mite in activeMites) {
      if (mite.cxt == sem) {
        val value = mite["value"]
        val convertedValue: Any? = if (value is Variable) var2Frames[value.base] else value
        val frame = var2Frames[(mite["frame"] as Variable).base]!!

        frameSections.getOrPut(frame, { section })
        if (convertedValue is Frame) {
          frameSections.getOrPut(convertedValue as Frame, { section })
        }

        val assignment = Assignment(frame, mite["attr"] as String, convertedValue!!)
        assignments.add(assignment)
        assignmentSections[assignment] = section
      }
      else if (mite.cxt == semSectionEnd) {
        section++
      }
    }
  }

  fun getFrames() = var2Frames.values().toList()

  fun presentable(): String {
    val frameId = HashMap<Frame, String>()
    var counter = 'A'
    val namer = { (frame: Frame, curSection: Int) ->
      val letter = frameId.getOrPut(frame) { "${counter++}" }
      val frameSection = frameSections[frame]!!
      if (frameSection == curSection) letter else letter + "@" + frameSection
    }

    var result = ""
    var lastSection = 1
    for (a in assignments) {
      val section = assignmentSections[a]!!
      if (section != lastSection) {
        counter = 'A'
        result += "-- $section:\n"
        lastSection = section
      }

      result += namer(a.frame, section) + ".${a.property}="
      result += if (a.value is Frame) namer(a.value, section) else a.value as String
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

