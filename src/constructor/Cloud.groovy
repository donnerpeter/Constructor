package constructor

/**
 * @author peter
 */
class Cloud {
  def comparator = { c1, c2 -> ranges[c1].fromInt - ranges[c2].fromInt} as Comparator
  Map<Construction, Set<Construction>> usages = [:]
  Map<Construction, IntRange> ranges = [:]
  Map<List, Closure> expectations = [:]
  Set<List> recalcExpectations = new LinkedHashSet<List>()
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

    checkExpectations(expectations.keySet())

    if (active.contains(c)) {
      def ctx = new ParsingContext(cloud: this)
      c.activate(ctx)

      expectations.putAll(ctx.expectations)

      checkExpectations(ctx.expectations.keySet())
    }
  }

  private def checkExpectations(Set<List> toRecalc) {
    recalcExpectations.addAll(toRecalc)
    while (recalcExpectations) {
      List pattern = recalcExpectations.iterator().next()
      recalcExpectations.remove(pattern)
      def lists = match(pattern)
      if (lists) {
        def action = expectations.remove(pattern)
        lists.each {
          int _min = Integer.MAX_VALUE
          int _max = Integer.MIN_VALUE
          it.each {
            _min = Math.min(_min, ranges[it].fromInt)
            _max = Math.max(_max, ranges[it].toInt)
            promote(it)
          }
          addConstruction(action(it), _min.._max)
        }
      }
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
    active.each {c ->
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
    active.each {c ->
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



  List<List<Construction>> match(List pattern) {
    def result = []
    if (pattern[0] instanceof Construction) {
      def pos = ranges[pattern[0]].toInt
      def next = findAfter(pattern[1], pos)
      if (next) {
        if (pattern.size() > 2) {
          def nnext = findAfter(pattern[2], ranges[next].toInt)
          if (nnext) {
            result << [pattern[0], next, nnext]
          }
        } else {
          result << [pattern[0], next]
        }
      }
    }
    else if (pattern[1] instanceof Construction) {
      def pos = ranges[pattern[1]].fromInt
      def prev = findBefore(pattern[0], pos)
      if (prev) {
        result << [prev, pattern[1]]
      }
    }
    result
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
