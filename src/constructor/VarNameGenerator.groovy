package constructor

/**
 * @author peter
 */
class VarNameGenerator {
  private int curVarIndex = 0
  private Map<Construction, String> varNames = [:]
  Set<Construction> reduced
  Cloud cloud

  def VarNameGenerator(Cloud cloud) {
    reduced = cloud.reduce()
    this.cloud = cloud
  }

  boolean isUsage(Construction c) {
    varNames[c] != null
  }

  def prefix(Construction c) {
    if (isUsage(c)) {
      return varNames[c]
    }
    def useCount = cloud.usages[c].findAll { reduced.contains(it) }.size()
    if (!(c instanceof Colored) && cloud.usages[c].findAll { cloud.colors[c] != cloud.colors[it] }) {
      useCount++
    }
    if (useCount < 2) {
      return ""
    }
    return (varNames[c] = "#${++curVarIndex}") + "="
  }

}
