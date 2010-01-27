package constructor

/**
 * @author peter
 */
class Cloud {
  def comparator = { c1, c2 -> starts[c1] - starts[c2]} as Comparator
  Map<Construction, Set<Construction>> usages = [:]
  Map<Construction, Integer> starts = [:]

  def addConstruction(Construction c, int at) {
    if (usages[c]) {
      return
    }

    starts[c] = at
    usages.get(c, new HashSet())
    c.args.each {arg ->
      addConstruction(arg, -1)
      usages[arg] << c
    }
  }

  def prettyPrint() {
    int curVarIndex = 0
    Map<Construction, String> varNames = [:]
    Closure varNameGenerator = {c ->
      if (varNames[c]) {
        return varNames[c] + ""
      }
      if (usages[c].size() < 2) {
        return ""
      }
      return (varNames[c] = "#${++curVarIndex}") + "="
    }

    def roots = usages.keySet().sort(comparator).findAll {usages[it].isEmpty()}

    StringBuilder sb = new StringBuilder()
    roots.each { c ->
      sb.append(c.prettyPrint(varNameGenerator, "")).append("\n")
    }
    return sb.toString()
  }

}