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
  Set<Construction> finished = []
  Set<Construction> weak = []
  Map<Construction, Set<Construction>> competitors = [:]
  Set expectations = [] as Set
  LinkedHashMap<Construction, Boolean> active = new LinkedHashMap<Construction, Boolean>()
  int maxColor = 0

  def addConstructions(List<Construction> alternatives, IntRange range) {
    def inputSet = alternatives as Set
    alternatives.each {
      initConstruction(it, range)
      competitors[it] = inputSet
    }

    Collection<Construction> filtered = filterInitially(alternatives).findAll { shouldActivate(it) }
    if (filtered) {
      expectations.clear()

      LinkedList<Construction> queue = new LinkedList<Construction>(filtered)
      while (queue) {
        def c = queue.removeFirst()
        if (!shouldActivate(c)) {
          continue
        }

        promote c
        if (c.tracked) {
          println "added $c; active=${this.active}"
          c
        }
        updateActive(queue, inputSet.contains(c))
        updateActive(queue, false)
      }
    }
  }

  private Collection<Construction> filterInitially(List<Construction> alternatives) {
    if (alternatives.size() > 1) {
      def expected = alternatives.findAll { cons -> expectations.any { msg -> cons.isAccepted(msg, this) } }
      if (!expected.isEmpty() && expected.size() < alternatives.size()) {
        weak += (alternatives - expected)
        return expected
      }
    }

    return alternatives
  }

  private boolean shouldActivate(Construction c) {
    return !isWeak(c) && c.descr.shouldActivate()
  }

  def updateActive(LinkedList<Construction> queue, boolean skipFirst) {
    (active.keySet() as List).reverse().each { Construction ac ->
      if (skipFirst) {
        skipFirst = false
        return
      }

      if (!active.containsKey(ac)) {
        return //todo iterate over actual active set
      }

      def usedArguments = [] as Set
      def ctx = new ParsingContext(ac, this)
      def happy = ac.descr.activate(ac, ctx)
      ctx.newConstructions.each {newC ->
        initConstruction(newC, compositeRange(newC))
        queue << newC
        newC.descr.demotedArgs.each { i -> demote newC.args[i] }
        usedArguments += newC.args
      }
      ctx._demoted.each { demote it }

      active[ac] = happy

      usedArguments.each {
        def comp = competitors[it]
        (comp - usedArguments).each {
          demote it
          weaken(it)
        }
      }
    }
  }

  private weaken(Construction c) {
    weak << c
    usages[c].each { weaken(it) }
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
    competitors[c] = [c] as Set
    c.args.each { usages[it] << c }
  }

  def prettyPrint() {
    prettyPrint(0, "", new VarNameGenerator(this))
  }

  def reduce() {
    def toExclude = [] as Set
    def all = usages.keySet()
    all.each { it ->
      if (it.name == "Space" || isWeak(it)) {
        toExclude << it
      }
    }
    return (all-toExclude).sort(comparator)
  }

  boolean isWeak(Construction c) {
    return c in weak
  }

  def prettyPrint(int color, String indent, VarNameGenerator varNameGenerator) {
    def roots = reduce().findAll {
      colors[it]==color && !(it instanceof Colored) && usages[it].findAll { colors[it]==color }.isEmpty()
    }

    StringBuilder sb = new StringBuilder()
    roots.each {c ->
      if (sb.size() > 0) {
        sb.append("\n")
      }
      sb.append(indent).append(c.prettyPrint(varNameGenerator, indent, this))
    }
    return sb.toString()
  }

  private List<Construction> plausibleAlternatives(Construction c) {
    List<Construction> all = competitors[c] as List
    def used = all.findAll { !isWeak(it) } as List
    assert used.size() > 0 : c
    return used
  }

  Construction findAfter(hint, int pos) {
    def result = null
    active.keySet().each {c ->
      def p = ranges[c].fromInt
      if (p >= pos) {
        def cand = plausibleAlternatives(c).findAll {it.isAccepted(hint, this)}
        if (cand) {
          result = cand[0]
        }
      }
    }
    if (!result) {
      expectations << hint
    }
    return result
  }

  List<Construction> activeBefore(int pos) {
    def result = []
    active.keySet().each {
      if (ranges[it].toInt <= pos) {
        result += plausibleAlternatives(it)
      }
    }
    return result
  }

  Construction findBefore(hint, int pos) {
    def result = null
    activeBefore(pos).each { Construction c ->
      def p = ranges[c].fromInt
      if (c.isAccepted(hint, this)) {
        if (result && ranges[result].fromInt > p) {
          return
        }
        result = c
      }
    }
    return result
  }

  Collection<Construction> allAt(int pos, boolean after) {
    usages.keySet().findAll { pos == (after ? ranges[it].fromInt : ranges[it].toInt) }
  }

  List<List<Construction>> match(Construction cur, List pattern) {
    def pivot = pattern.indexOf("_")
    List<Construction> result = new Construction[pattern.size()] as List
    result[pivot] = cur
    def c = cur
    for (int i = pivot - 1; i >= 0; i--) {
      def prev = findBefore(pattern[i], ranges[c].fromInt)
      if (!prev) return []
      result[i] = c = prev
    }
    c = cur
    for (i in pivot + 1..<pattern.size()) {
      def next = findAfter(pattern[i], ranges[c].toInt)
      if (!next) {
        expectations << pattern[i]
        return []
      }
      result[i] = c = next
    }
    return [result]
  }

  def promote(Construction c) {
    if (c.tracked) {
      c
    }
    def old = active.remove(c)
    if (active.size() >= 7) {
      Construction weak = weakest()
      if (weak.tracked) {
        println "Removing $weak, active=$active"
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
      int newColor = ++maxColor
      colorRanges[range] = c = new Colored(newColor)
      initConstruction(c, range)
      ranges.each {ec, r ->
        if (r.fromInt >= range.fromInt && r.toInt <= range.toInt && !colors[ec]) {
          colors[ec] = newColor
        }
      }
    }
    return c
  }

  private def semantics(Construction c, Map sem) {
    if (sem.containsKey(c)) {
      return sem[c]
    }

    def args = c.children(this)
    def result = c.descr.buildSemantics(new Object() {
      def size() { args.size() }
      def getAt(int i) { semantics(args[i], sem) }
    })
    sem[c] = result
    return result
  }

  String semantics() {
    Map frames = [:]
    reduce().each { semantics(it, frames) }
    return new FramePrinter(frames.values().findAll {it instanceof Frame}).prettyPrint("")
  }
}
