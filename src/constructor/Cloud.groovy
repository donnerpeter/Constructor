package constructor

/**
 * @author peter
 */
class Cloud {
  def comparator = { Construction c1, Construction c2 ->
    return ranges[c1].fromInt - ranges[c2].fromInt ?:
        ranges[c1].toInt - ranges[c2].toInt ?:
        c1.descr.name.compareTo(c2.descr.name)
  } as Comparator
  Map<Construction, Set<Construction>> usages = [:]
  Map<Construction, IntRange> ranges = [:]
  Map<Construction, Integer> colors = [:]
  Map<IntRange, Colored> colorRanges = [:]
  Set<Construction> finished = []
  Set<Construction> weak = []
  Map<Construction, Set<Construction>> competitors = [:]
  Set expectations = [] as Set
  LinkedList<Construction> active = new LinkedList<Construction>()
  Set<Construction> touched = [] as Set
  LinkedHashSet<Construction> fresh = [] as LinkedHashSet
  Set<Construction> processed = [] as LinkedHashSet
  int maxColor = 0

  void addConstructions(List<Construction> alternatives, IntRange range) {
    def inputSet = alternatives as Set
    alternatives.each { c ->
      ranges[c] = range
      usages[c] = [] as Set
      colors[c] = 0
      c.args.each { usages[it] << c }
      competitors[c] = inputSet
      fresh << c
      if (c.tracked) {
        println "Adding $c"
      }
      c.descr.demotedArgs.each { demote(c.args[it]) }
    }

    def usedArgs = [] as Set
    alternatives.each { usedArgs += it.args }
    usedArgs.each {
      (competitors[it] - usedArgs).each { weaken it } 
    }

    def capable = alternatives.find { shouldActivate(it) }
    if (capable) {
      promote capable
    }

    processed.clear()
  }

  private Construction nextUnprocessed() {
    for (c in active - processed) {
      Collection<Construction> chosen = competitors[c].findAll { shouldActivate(it) }
      if (chosen.size() == 1) {
        return chosen.iterator().next()
      }
      //todo activate competing alternatives if only they remain
    }

    return null
  }

  void markFinished(Construction construction) {
    finished << construction
    processed.remove construction
  }

  void updateActive() {
    while (true) {
      def c = nextUnprocessed()
      if (!c) break

      processed << c
      touched.remove c

      c.descr.activate(c, new ParsingContext(c, this))
    }

    def usedArgs = [] as Set
    fresh.each { usedArgs += it.args }
    usedArgs.each {
      (competitors[it] - usedArgs).each { weaken it } 
    }

    fresh.clear()
    expectations.clear()
  }

  private boolean shouldActivate(Construction c) {
    return !isWeak(c) && c.descr.shouldActivate()
  }

  private weaken(Construction c) {
    weak << c
    usages[c].each { weaken(it) }
    demote c
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

  private Construction findAfter(hint, int pos, Function1<Construction, Boolean> filter) {
    def result = null
    active.each {c ->
      def p = ranges[c].fromInt
      if (p >= pos) {
        def cand = plausibleAlternatives(c).findAll { filter(it) && it.isAccepted(hint, this)}
        if (result && ranges[result].fromInt < p) {
          return
        }

        if (cand) {
          result = cand[0]
        }
      }
    }
    return result
  }

  private Construction findBefore(hint, int pos, Function1<Construction, Boolean> filter) {
    def result = null
    active.each {c ->
      def p = ranges[c].toInt
      if (p <= pos) {
        def cand = plausibleAlternatives(c).findAll { filter(it) && it.isAccepted(hint, this)}
        if (result && ranges[result].toInt > p) {
          return
        }

        if (cand) {
          result = cand[0]
        }
      }
    }
    return result
  }

  Collection<Construction> allAt(int pos, boolean after) {
    usages.keySet().findAll { pos == (after ? ranges[it].fromInt : ranges[it].toInt) }
  }

  List<Construction> match(Construction cur, List pattern, boolean serious = true, Function1<Construction, Boolean> filter) {
    def pivot = pattern.indexOf("_")
    List<Construction> result = new Construction[pattern.size()] as List
    result[pivot] = cur
    def c = cur
    for (int i = pivot - 1; i >= 0; i--) {
      def prev = findBefore(pattern[i], ranges[c].fromInt, filter)
      if (!prev) return null
      result[i] = c = prev
    }
    c = cur
    for (i in pivot + 1..<pattern.size()) {
      def next = findAfter(pattern[i], ranges[c].toInt, filter)
      if (!next) {
        if (serious) {
          touched += result[0..<i]
        }
        expectations << pattern[i]
        return null
      }
      result[i] = c = next
    }
    if (serious) {
      result.each { promote it }
    }
    return result
  }

  private String cons2str(c) {
    return c.toString()
//    def r = ranges[c]
//    "$c:$r.fromInt..$r.toInt"
  }

  def promote(Construction c) {
    if (c.tracked) {
      println "Promoting ${cons2str(c)}, active=$active"
    }
    active.remove(c)
    if (active.size() >= 8) { //todo WM size should be 7
      Construction victim = weakest()
      if (victim.tracked) {
        println "Removing ${cons2str(victim)}, active=$active"
      }
      demote victim
    }
    active.addFirst c
    touched << c
  }

  Construction weakest() {
    def untouched = active - touched
    if (untouched) {
      return untouched[-1]
    }

    return active[-1]
  }

  def demote(Construction c) {
    active.remove c
    touched.remove c
  }

  Colored coloredRange(IntRange range) {
    def c = colorRanges[range]
    if (!c) {
      int newColor = ++maxColor
      colorRanges[range] = c = new Colored(newColor)
      addConstructions([c], range)
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
