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
      if (usages[arg] == null) {
        assert usages[arg]
      }
      usages[arg] << c
    }

    promote(c)

    expectations.clone().each {pattern, action ->
      if (expectations.containsKey(pattern)) {
        match(pattern, action)
      }
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

  def findAfter(hint, int pos) {
    def result = null
    active.clone().each {c ->
      def p = starts[c]
      if (p >= pos && isAccepted(hint, c)) {
        result = c
      }
    }
    return result
  }

  private def isAccepted(hint, Construction c) {
    if (hint instanceof Class) {
      hint.isInstance(c)
    } else {
      c.ping(hint)
    }
  }

  def findBefore(hint, int pos) {
    def result = null
    active.clone().each {c ->
      def p = starts[c]
      if (p + c.name.size() <= pos && isAccepted(hint, c)) {
        if (result && starts[result] > p) {
          return
        }
        result = c
      }
    }
    return result
  }

  private def executeAction(action, args) {
    args.each { promote(it) }
    addConstruction action(args), starts[args[0]]
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
            executeAction action, [pattern[0], next, nnext]
          }
        } else {
          expectations.remove pattern
          executeAction action, [pattern[0], next]
        }
      }
    }
    else if (pattern[1] instanceof Construction) {
      def pos = starts[pattern[1]]
      def prev = findBefore(pattern[0], pos)
      if (prev) {
        expectations.remove pattern
        executeAction action, [prev, pattern[1]]
      }
    }
  }

  def promote(Construction c) {
    demote(c)
    active.addFirst(c)
    if (active.size() > 7) {
      active.removeLast()
    }
  }

  def demote(Construction c) {
    active.remove(c)
  }
}