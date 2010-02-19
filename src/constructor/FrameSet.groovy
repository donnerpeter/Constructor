package constructor

/**
 * @author peter
 */
class FrameSet {
  Set<Frame> frames = new LinkedHashSet()
  Map<Frame, Set<Frame>> usages = [:]

  String prettyPrint() {
    def gen = new FrameNameGenerator(this)

    def roots = frames.findAll { usages[it].isEmpty() && !it.attributes.isEmpty() }

    StringBuilder sb = new StringBuilder()
    roots.each {f ->
      if (sb.size() > 0) {
        sb.append("\n")
      }
      sb.append(f.prettyPrint(gen, ""))
    }
    return sb.toString()
  }

  def addFrame(Frame frame) {
    if (frame in frames) {
      return
    }

    frame.attributes.each { name, value ->
      if (value instanceof Frame) {
        addFrame(value)
        usages[value] << frame
      }
    }


    frames << frame
    usages[frame] = [] as Set
  }
}
