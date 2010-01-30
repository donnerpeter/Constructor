package constructor

/**
 * @author peter
 */
class Cloud {
  def comparator = { c1, c2 -> starts[c1] - starts[c2]} as Comparator
  Map<Construction, Set<Construction>> usages = [:]
  Map<Construction, Integer> starts = [:]
  Map<List, Closure> expectations = [:]
  LinkedList<Construction> active = new LinkedList<Construction>()

  def addConstruction(Construction c, int at) {
    starts[c] = at
    usages[c] = [] as Set
    c.args.each {arg ->
      usages[arg] << c
    }

    active.addFirst(c)
    if (active.size() > 7) {
      active.removeLast()
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

  def findAfter(Class hint, int pos) {
    def result = null
    active.clone().each {c ->
      def p = starts[c]
      if (p >= pos && hint.isInstance(c)) {
        result = c
      }
    }
    return result
  }

  def match(List pattern, Closure action) {
    expectations[pattern] = action
    if (pattern[0] instanceof Construction) {
      def pos = starts[pattern[0]] + pattern[0].name.size()
      def next = findAfter(pattern[1], pos)
      if (next) {
        if (pattern.size() > 2) {
          def nnext = findAfter(pattern[2], starts[next] + next.name.size())
          if (nnext) {
            expectations.remove pattern
            addConstruction action([pattern[0], next, nnext]), pos
          }
        } else {
          expectations.remove pattern
          addConstruction action([pattern[0], next]), pos
        }
      }
    }
    else if (pattern[1] instanceof Construction) {
      def pos = starts[pattern[1]]
      active.clone().each {c ->
        def p = starts[c]
        if (p + c.name.size() <= pos && pattern[0].isInstance(c)) {
          expectations.remove pattern
          addConstruction action([c, pattern[1]]), p
        }
      }
    }
  }
}