package constructor

/**
 * @author peter
 */
class Cloud {
  def comparator = { c1, c2 ->
    return ranges[c1].fromInt - ranges[c2].fromInt ?: ranges[c1].toInt - ranges[c2].toInt
  } as Comparator
  Map<Construction, Set<Construction>> usages = [:]
  Map<Construction, IntRange> ranges = [:]
  Map<Construction, Integer> colors = [:]
  Map<List, Closure> expectations = [:]
  Set<List> recalcExpectations = new LinkedHashSet<List>()
  LinkedList<Construction> active = new LinkedList<Construction>()
  int curColor = 0, maxColor = 0

  def addConstruction(Construction c, IntRange range) {
    ranges[c] = range
    usages[c] = [] as Set
    c.args.each {arg ->
      if (usages[arg] == null) {
        assert usages[arg]
      }
      usages[arg] << c
    }
    colors[c] = curColor

    if (c instanceof Colored) {
      return
    }

    promote(c)

    checkExpectations(expectations.keySet())

    if (active.contains(c)) {
      def ctx = new ParsingContext(cloud: this)
      c.activate(ctx)

      if (ctx.oldColored.size() > 0) {
        curColor = 0
        ctx.oldColored.each { colors[it] = curColor }
      }

      expectations.putAll(ctx.expectations)

      checkExpectations(ctx.expectations.keySet())
    }
  }

  Colored pushColor(int pos) {
    maxColor++
    curColor = maxColor
    def colored = new Colored(curColor)
    addConstruction(colored, pos..pos)
    return colored
  }

  private def checkExpectations(Set<List> toRecalc) {
    recalcExpectations.addAll(toRecalc)
    while (recalcExpectations) {
      List pattern = recalcExpectations.iterator().next()
      recalcExpectations.remove(pattern)
      def lists = match(pattern)
      if (lists) {
        def action = expectations.remove(pattern)
        lists.each { argList ->
          int _min = Integer.MAX_VALUE
          int _max = Integer.MIN_VALUE
          argList.each { arg ->
            _min = Math.min(_min, ranges[arg].fromInt)
            _max = Math.max(_max, ranges[arg].toInt)
            promote(arg)
          }
          addConstruction(action(argList), _min.._max)
        }
      } else {
        def anchor = pattern.findIndexOf { it instanceof Construction }
        if (anchor > 0 && !activeBefore(ranges[pattern[anchor]].fromInt)) {
          expectations.remove(pattern)
        }
      }
    }
  }

  def prettyPrint() {
    prettyPrint(0, "")
  }

  def prettyPrint(int color, String indent) {
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

    def roots = usages.keySet().sort(comparator).findAll {usages[it].isEmpty() && colors[it]==color }

    StringBuilder sb = new StringBuilder()
    roots.each {c ->
      if (sb.size() > 0) {
        sb.append("\n")
      }
      sb.append(indent).append(c.prettyPrint(varNameGenerator, indent, this))
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
    if (hint instanceof List) {
      return hint.every { isAccepted(it, c) }
    } else {
      c.ping(hint)
    }
  }

  def activeBefore(int pos) {
    active.findAll { ranges[it].toInt <= pos }
  }

  def findBefore(hint, int pos) {
    def result = null
    activeBefore(pos).each {c ->
      def p = ranges[c].fromInt
      if (isAccepted(hint, c)) {
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
    else if (pattern.size() == 4 && pattern[3] instanceof Construction) {
        def prev2 = findBefore(pattern[2], ranges[pattern[3]].fromInt)
        if (prev2) {
          def prev1 = findBefore(pattern[1], ranges[prev2].fromInt)
          if (prev1) {
            def prev0 = findBefore(pattern[0], ranges[prev1].fromInt)
            if (prev0) {
              result<< [prev0, prev1, prev2, pattern[3]]
            }

          }
        }
      }
    result
  }

  def promote(Construction c) {
    active.remove(c)
    if (active.size() >= 7) {
      Construction weak = weakest()
      if (weak.tracked) {
        weakest()
      }
      active.remove(weak)
      clearExpectations(weak)
    }
    active.addFirst(c)
  }

  Construction weakest() {
    def happy = active.findAll { isHappy(it) }
    if (!happy.isEmpty()) {
      def infamous = happy.findAll { !it.famous }
      if (!infamous.isEmpty()) {
        return infamous[-1]
      }

      return happy[-1]
    }

    return active.last()
  }

  def isHappy(Construction c) {
    return findExpectations(c).isEmpty()
  }

  def demote(Construction c) {
    active.remove(c)
    clearExpectations(c)
  }

  def findExpectations(Construction c) {
    expectations.keySet().findAll {k -> k.contains(c) }
  }

  private def clearExpectations(Construction c) {
    def exps = findExpectations(c)
    expectations.keySet().removeAll(exps)
    recalcExpectations.removeAll(exps)
  }
}
