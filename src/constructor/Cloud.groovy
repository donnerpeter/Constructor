package constructor

/**
 * @author peter
 */
class Cloud {
  def comparator = { Construction c1, Construction c2 ->
    return startOffset(c1) - startOffset(c2) ?:
        endOffset(c1) - endOffset(c2) ?:
        c1.descr.name.compareTo(c2.descr.name)
  } as Comparator
  Map<Construction, Set<Construction>> usages = [:]
  Map<Construction, IntRange> ranges = [:]
  Map<Construction, Integer> colors = [:]
  Map<IntRange, Colored> colorRanges = [:]
  Set<Construction> finished = []
  Set<Construction> weak = []
  Map<Construction, List<Construction>> competitors = [:]
  Set expectations = [] as Set
  LinkedList<Construction> active = new LinkedList<Construction>()
  Set<Construction> touched = [] as Set
  LinkedHashSet<Construction> fresh = [] as LinkedHashSet
  Set<Construction> processed = [] as LinkedHashSet
  int maxColor = 0

  void addConstructions(List<Construction> alternatives, IntRange range) {
    alternatives.each { initConstruction(it, range, alternatives) }

    List<Construction> backgrounded = []
    List<Construction> phrases = []
    alternatives.each {
      def phrase = it.descr.makePhrase(it, new ParsingContext(it, this))
      if (phrase) {
        backgrounded << it

        def pc = phrase.build([], this)
        pc.possibleHeads << it as String
        phrases << pc
        initConstruction pc, range, phrases

        def head = PhraseConstruction.HEAD.build([pc, it], this)
        initConstruction head, range, [head]
      }
    }


    weakenCompetitors(alternatives)

    def capable = (alternatives - backgrounded).find { shouldActivate(it) }
    if (capable) {
      promote capable
    }

    capable = phrases.find { shouldActivate(it) }
    if (capable) {
      promote capable
    }

    processed.clear()
  }

  private void initConstruction(Construction c, IntRange range, List<Construction> alternatives) {
    ranges[c] = range
    usages[c] = [] as Set
    colors[c] = 0
    c.args.each { usages[it] << c }
    competitors[c] = alternatives
    fresh << c
    if (c.tracked) {
      println "Adding $c"
    }
    c.descr.demotedArgs.each { demote(c.args[it]) }
  }

  private void weakenCompetitors(Collection<Construction> alternatives) {
    def usedArgs = [] as Set
    alternatives.each { allConnected it, usedArgs }
    usedArgs.each {
      (competitors[it] - usedArgs).each { weaken it }
    }
  }

  private void allConnected(Construction c, Set<Construction> visited) {
    if (!visited.add(c)) return

    c.children(this).each { allConnected it, visited }
    strongUsages(c, []).each { allConnected it, visited }
  }

  private List<Construction> nextUnprocessed() {
    for (c in active) {
      Collection<Construction> chosen = (competitors[c] - processed).findAll { shouldActivate(it) }
      if (chosen.size() == 1) {
        return chosen
      }
    }

    for (c in active) {
      Collection<Construction> chosen = (competitors[c] - processed).findAll { shouldActivate(it) }
      if (chosen) {
        return chosen
      }
    }

    return []
  }

  void markFinished(Construction construction) {
    finished << construction
    processed.remove construction
  }

  void updateActive() {
    while (true) {
      def c = nextUnprocessed()
      if (!c) break

      processed += c
      touched -= c

      c.each { it.descr.activate(it, new ParsingContext(it, this)) }
    }

    weakenCompetitors(fresh)

    fresh.clear()
    expectations.clear()
  }

  private boolean shouldActivate(Construction c) {
    return !isWeak(c) && c.descr.shouldActivate()
  }

  private void weaken(Construction c) {
    if (c in weak) return

    weak << c
    c.args.each { weaken it }
    usages[c].each { weaken it }
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
      else if (PhraseConstruction.project(it, this) != it || it.descr == PhraseConstruction.HEAD) {
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
      return colors[it]==color &&
              !(it instanceof Colored) &&
              strongUsages(it, []).findAll { colors[it]==color && it.descr != PhraseConstruction.HEAD }.isEmpty()
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
    def used = competitors[c].findAll { !isWeak(it) } as List
    assert used.size() > 0: c
    return used
  }

  private Construction findAfter(hint, int pos, Function1<Construction, Boolean> filter) {
    def result = null
    active.each {c ->
      def p = startOffset(c)
      if (p >= pos) {
        def cand = plausibleAlternatives(c).findAll { filter(it) && it.isAccepted(hint, this)}
        if (result && startOffset(result) < p) {
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
      def p = endOffset(c)
      if (p <= pos) {
        def cand = plausibleAlternatives(c).findAll { filter(it) && it.isAccepted(hint, this)}
        if (result && endOffset(result) > p) {
          return
        }

        if (cand) {
          result = cand[0]
        }
      }
    }
    return result
  }

  int endOffset(Construction c) {
    return ranges[c].toInt
  }

  int startOffset(Construction c) {
    return ranges[c].fromInt
  }

  Collection<Construction> allAt(int pos, boolean after) {
    usages.keySet().findAll { pos == (after ? startOffset(it) : endOffset(it)) }
  }

  List<Construction> match(Construction cur, List pattern, boolean serious = true, Function1<Construction, Boolean> filter) {
    def pivot = pattern.indexOf("_")
    List<Construction> result = new Construction[pattern.size()] as List
    result[pivot] = cur
    def c = cur
    for (int i = pivot - 1; i >= 0; i--) {
      def prev = findBefore(pattern[i], startOffset(c), filter)
      if (!prev) return null
      result[i] = c = prev
    }
    c = cur
    for (i in pivot + 1..<pattern.size()) {
      def next = findAfter(pattern[i], endOffset(c), filter)
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

  void promote(Construction c) {
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

  List<Construction> allUsages(Construction c, hint) {
    return usages[c].findAll { it.isAccepted(hint, this) } as List
  }

  List<Construction> strongUsages(Construction c, hint) {
    return allUsages(c, hint).findAll { !isWeak(it) }
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
      ranges.keySet().each {ec ->
        if (startOffset(ec) >= range.fromInt && endOffset(ec) <= range.toInt && !colors[ec]) {
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

    if (c instanceof PhraseConstruction) {
      def head = c.head(this)
      return head ? semantics(head, sem) : null
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
