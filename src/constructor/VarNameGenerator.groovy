package constructor

/**
 * @author peter
 */
class VarNameGenerator<T> {
  private int curVarIndex = 0
  private Map<T, String> varNames = [:]
  Set<T> reduced
  Cloud cloud

  def VarNameGenerator(Cloud cloud) {
    reduced = cloud.reduce() as Set
    this.cloud = cloud
  }

  boolean isUsage(Construction c) {
    varNames[c] != null
  }

  def prefix(Construction c) {
    if (isUsage(c)) {
      return varNames[c]
    }
    def usages = cloud.usages[c].findAll { reduced.contains(it) && PhraseConstruction.HEAD != it.descr }
    def useCount = usages.size()
    if (!(c instanceof Colored) && usages.findAll { cloud.colors[c] != cloud.colors[it] }) {
      useCount++
    }
    
    if (useCount < 2) {
      return ""
    }
    return (varNames[c] = "#${++curVarIndex}") + "="
  }

}
