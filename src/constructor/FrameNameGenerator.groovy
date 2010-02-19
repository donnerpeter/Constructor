package constructor

/**
 * @author peter
 */
//todo merge with VarNameGenerator
class FrameNameGenerator {
  private int curVarIndex = 0
  private Map<Object, String> varNames = [:]
  Set<Frame> reduced
  FrameSet frameSet

  def FrameNameGenerator(FrameSet input) {
    reduced = input.frames as Set
    frameSet = input
  }

  boolean isUsage(Frame c) {
    varNames[c] != null
  }

  def prefix(Frame c) {
    if (isUsage(c)) {
      return varNames[c]
    }
    def useCount = frameSet.usages[c].intersect(reduced).size()
    if (useCount < 2) {
      return ""
    }
    return (varNames[c] = "#${++curVarIndex}") + "="
  }

}