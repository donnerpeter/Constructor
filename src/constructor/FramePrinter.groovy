package constructor

/**
 * @author peter
 */
//todo merge with VarNameGenerator
class FramePrinter {
  private int curVarIndex = 0
  private Map<Object, String> varNames = [:]
  Set<Frame> frames = new LinkedHashSet()
  Set<Frame> coloredRoots = [] as Set
  Map<Frame, Set<Frame>> usages = [:]

  def FramePrinter(Collection<Frame> sem) {
    sem.each { addFrame(it) }
    coloredRoots = coloredRoots.findAll {usages[it].size() == 1} as Set
  }

  def addFrame(Frame frame) {
    if (frame in frames) {
      return
    }

    frames << frame
    frame.attributes.keySet().each { name ->
      def value = frame[name]
      if (value instanceof Frame) {
        addFrame(value)
        usages[value] << frame
      } else if (value instanceof List) {
        value.each {
          if (it instanceof Frame) {
            addFrame(it)
            usages[it] << frame
            coloredRoots << it
          }
        }
      }
    }
    usages[frame] = [] as Set
  }

  boolean isUsage(Frame c) {
    varNames[c] != null
  }

  String prettyPrint(String indent) {
    def roots = frames.findAll { usages[it].isEmpty() }
    return roots.collect {indent + it.prettyPrint(this, indent)}.join("\n")
  }

  def prefix(Frame c) {
    if (isUsage(c)) {
      return varNames[c]
    }
    if (c in coloredRoots) {
      return ""
    }
    def useCount = usages[c].size()
    if (useCount < 2) {
      return ""
    }
    curVarIndex++
    return (varNames[c] = "#${curVarIndex}") + "="
  }

}