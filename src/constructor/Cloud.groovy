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
  LinkedHashMap<Construction, Boolean> active = new LinkedHashMap<Construction, Boolean>()
  int maxColor = 0

  def addConstructions(List<Construction> competitors, IntRange range) {
    competitors.each { initConstruction(it, range) }

    LinkedList<Construction> queue = new LinkedList<Construction>(competitors)

    while (queue) {
      def c = queue.removeFirst()
      if (!c.descr.shouldActivate()) {
        continue
      }

      promote c
      if (c.tracked) {
        println "added $c; active=${this.active}"
        c
      }
      updateActive(queue, competitors.contains(c))
      updateActive(queue, false)
    }
  }

  def updateActive(LinkedList<Construction> queue, boolean skipFirst) {
    def toDemote = []
    (active.keySet() as List).reverse().each { Construction ac ->
      if (skipFirst) {
        skipFirst = false
        return
      }

      def ctx = new ParsingContext(ac, this)
      def happy = ac.descr.activate(ac, ctx)
      ctx.newConstructions.each {newC ->
        initConstruction(newC, compositeRange(newC))
        queue << newC
        newC.descr.demotedArgs.each { i -> toDemote << newC.args[i] }
      }

      if (ac.tracked) {
        if (!happy) {
          ac.descr.activate(ac, ctx)
        }
        if (happy) {
          ac.descr.activate(ac, ctx)
        }
      }
      active[ac] = happy

    }
    toDemote.each { demote it }
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
    c.args.each { usages[it] << c }
  }

  def prettyPrint() {
    int curVarIndex = 0
    Map<Construction, String> varNames = [:]
    def reduced = reduce()
    Closure varNameGenerator = {c ->
      if (varNames[c]) {
        return varNames[c] + ""
      }
      def useCount = usages[c].findAll { reduced.contains(it) }.size()
      if (!(c instanceof Colored) && usages[c].findAll { colors[c] != colors[it] }) {
        useCount++
      }
      if (useCount < 2) {
        return ""
      }
      return (varNames[c] = "#${++curVarIndex}") + "="
    }

    prettyPrint(0, "", varNameGenerator)
  }

  def reduce() {
    def toExclude = [] as Set
    def all = usages.keySet()
    all.each { it ->
      if (it.name == "Space") {
        toExclude << it
      }

      toExclude += it.descr.incompatible(it, new ParsingContext(it, this))
    }
    return all-toExclude-weak
  }

  def prettyPrint(int color, String indent, Closure varNameGenerator) {
    def roots = reduce().sort(comparator).findAll {
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

  Construction findAfter(hint, int pos) {
    def result = null
    active.keySet().each {c ->
      def p = ranges[c].fromInt
      if (p >= pos && c.isAccepted(hint, this)) {
        result = c
      }
    }
    return result
  }

  Collection<Construction> activeBefore(int pos) {
    active.keySet().toArray().findAll { ranges[it].toInt <= pos }
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
    for (int i = pivot + 1; i < pattern.size(); i++) {
      def next = findAfter(pattern[i], ranges[c].toInt)
      if (!next) return []
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
        println "Removing $c, active=$active"
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
        if (r.fromInt >= range.fromInt && r.toInt <= range.toInt && !colors[ec]) {
          colors[ec] = newColor
        }
      }
    }
    return c
  }
}
