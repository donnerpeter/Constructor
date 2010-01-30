package constructor

/**
 * @author peter
 */
class Cloud {
  def comparator = { c1, c2 -> starts[c1] - starts[c2]} as Comparator
  Map<Construction, Set<Construction>> usages = [:]
  Map<Construction, Integer> starts = [:]
  Map<List, Closure> expectations = [:]

  def addConstruction(Construction c, int at) {
    starts[c] = at
    usages[c] = [] as Set
    c.args.each {arg ->
      usages[arg] << c
    }

    expectations.clone().each {pattern, action ->
      match(pattern, action)
    }

    c.activate(new ParsingContext(cloud:this))
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

  def findBefore(Class hint, int pos) {
    def result = null
    starts.each {c, p ->
      if (p + c.name.size() <= pos && hint.isInstance(c)) {
        result = c
      }
    }
    return result
  }

  def match(List pattern, Closure action) {
    expectations[pattern] = action
    def result = false
    if (pattern[0] instanceof Construction) {
      def pos = starts[pattern[0]] + pattern[0].name.size()
      starts.clone().each {c, p ->
        if (p >= pos && pattern[1].isInstance(c)) {
          expectations.remove pattern
          addConstruction action([pattern[0], c]), pos
        }
      }
    }
    else if (pattern[1] instanceof Construction) {
      def pos = starts[pattern[1]]
      starts.clone().each {c, p ->
        if (p + c.name.size() <= pos && pattern[0].isInstance(c)) {
          expectations.remove pattern
          addConstruction action([c, pattern[1]]), p
        }
      }
    }
  }
}