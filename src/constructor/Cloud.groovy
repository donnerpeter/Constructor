package constructor

/**
 * @author peter
 */
class Cloud {
  def comparator = { c1, c2 -> ranges[c1].fromInt - ranges[c2].fromInt} as Comparator
  Map<Construction, Set<Construction>> usages = [:]
  Map<Construction, IntRange> ranges = [:]
  Map<List, Closure> expectations = [:]
  LinkedList<Construction> active = new LinkedList<Construction>()

  def addConstruction(Construction c, IntRange range) {
    ranges[c] = range
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

    if (active.contains(c)) {
      c.activate(new ParsingContext(cloud:this))
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

  def findAfter(hint, int pos) {
    def result = null
    active.clone().each {c ->
      def p = ranges[c].fromInt
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
      def p = ranges[c].fromInt
      if (ranges[c].toInt <= pos && isAccepted(hint, c)) {
        if (result && ranges[result].fromInt > p) {
          return
        }
        result = c
      }
    }
    return result
  }

  private def executeAction(action, args) {
    int _min = Integer.MAX_VALUE
    int _max = Integer.MIN_VALUE
    args.each {
      _min = Math.min(_min, ranges[it].fromInt)
      _max = Math.max(_max, ranges[it].toInt)
      promote(it)
    }
    addConstruction action(args), _min.._max
  }

  def match(List pattern, Closure action) {
    expectations[pattern] = action
    if (pattern[0] instanceof Construction) {
      def pos = ranges[pattern[0]].toInt
      def next = findAfter(pattern[1], pos)
      if (next) {
        if (pattern.size() > 2) {
          def nnext = findAfter(pattern[2], ranges[next].toInt)
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
      def pos = ranges[pattern[1]].fromInt
      def prev = findBefore(pattern[0], pos)
      if (prev) {
        expectations.remove pattern
        executeAction action, [prev, pattern[1]]
      }
    }
  }

  def promote(Construction c) {
    active.remove(c)
    active.addFirst(c)
    if (active.size() > 7) {
      def last = active.removeLast()
      clearExpectations(last)
    }
  }

  def demote(Construction c) {
    active.remove(c)
    clearExpectations(c)
  }

  private def clearExpectations(Construction c) {
    expectations.clone().each {k, v -> if (k.contains(c)) expectations.remove(k) }
  }
}