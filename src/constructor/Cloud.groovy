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
  Map<IntRange, Colored> colorRanges = [:]
  LinkedHashMap<Construction, Boolean> active = new LinkedHashMap<Construction, Boolean>()
  int maxColor = 0

  def addConstruction(Construction _c, IntRange range) {
    initConstruction(_c, range)

    LinkedList<Construction> queue = new LinkedList<Construction>()
    queue << _c
    
    while (queue) {
      def c = queue.removeFirst()
      if (!c.shouldActivate()) {
        continue
      }

      promote c
      if (c.tracked) {
        c
      }
      def toDemote = []
      active.keySet().each {ac ->
        def ctx = new ParsingContext(cloud: this)
        def happy = ac.activate(ctx)
        if (!happy) {
          happy
        }
        active[ac] = happy
        ctx.newConstructions.each {newC ->
          initConstruction(newC, compositeRange(newC))
          queue << newC
          newC.descr.demotedArgs.each { i -> toDemote << newC.args[i] }
        }
      }
      toDemote.each { demote it }
    }
  }

  private IntRange compositeRange(newC) {
    int _min = Integer.MAX_VALUE
    int _max = Integer.MIN_VALUE
    newC.args.each {arg ->
      _min = Math.min(_min, ranges[arg].fromInt)
      _max = Math.max(_max, ranges[arg].toInt)
    }
    return _min.._max
  }

  private def initConstruction(Construction c, IntRange range) {
    ranges[c] = range
    usages[c] = [] as Set
    colors[c] = 0
    c.args.each {arg ->
        if (usages[arg] == null) {
          assert usages[arg]
        }
        usages[arg] << c
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

  Construction findAfter(hint, int pos) {
    def result = null
    active.keySet().each {c ->
      def p = ranges[c].fromInt
      if (p >= pos && isAccepted(hint, c)) {
        result = c
      }
    }
    return result
  }

  def isAccepted(hint, Construction c) {
    if (hint instanceof List) {
      return hint.every { isAccepted(it, c) }
    } else {
      c.descr.ping(c, hint)
    }
  }

  def activeBefore(int pos) {
    active.keySet().toArray().findAll { ranges[it].toInt <= pos }
  }

  Construction findBefore(hint, int pos) {
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
      def next1 = findAfter(pattern[1], pos)
      if (next1) {
        if (pattern.size() > 2) {
          def next2 = findAfter(pattern[2], ranges[next1].toInt)
          if (next2) {
            if (pattern.size() > 3) {
              def next3 = findAfter(pattern[3], ranges[next1].toInt)
              if (next3) {
                result << [pattern[0], next1, next2, next3]
              }
            } else {
              result << [pattern[0], next1, next2]
            }
          }
        } else {
          result << [pattern[0], next1]
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
    else if (pattern.size() == 3 && pattern[2] instanceof Construction) {
      def prev1 = findBefore(pattern[1], ranges[pattern[2]].fromInt)
      if (prev1) {
        def prev0 = findBefore(pattern[0], ranges[prev1].fromInt)
        if (prev0) {
          result<< [prev0, prev1, pattern[2]]
        }
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
    if (c.tracked) {
      c
    }
    def old = active.remove(c)
    if (active.size() >= 7) {
      Construction weak = weakest()
      if (weak.tracked) {
        weakest()
      }
      active.remove(weak)
    }
    active[c] = old
  }

  Construction weakest() {
    def oldestFirst = active.keySet().toArray()
    def colored = oldestFirst.findAll { colors[it] > 0 }
    if (!colored.isEmpty()) {
      return colored[0]
    }

    def happy = oldestFirst.findAll { active[it] }
    if (!happy.isEmpty()) {
      return happy[0]
    }

    return active.keySet().iterator().next()
  }

  def demote(Construction c) {
    active.remove(c)
  }

  Colored coloredRange(IntRange range) {
    def c = colorRanges[range]
    if (!c) {
      def newColor = ++maxColor
      colorRanges[range] = c = new Colored(newColor)
      initConstruction(c, range)
      ranges.each {ec, r ->
        if (r.fromInt >= range.fromInt && r.toInt <= range.toInt) {
          colors[ec] = newColor
        }
      }
    }
    return c
  }
}
