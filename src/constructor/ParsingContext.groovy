package constructor

/**
 * @author peter
 */
class ParsingContext {
  Construction relativeTo
  Cloud cloud
  List<Construction> newConstructions = []
  Set<Construction> _demoted = []
  private Set<Construction> usedArgs = [] as Set
  private Set<Construction> relaxed = [] as Set

  def ParsingContext(Construction relativeTo, Cloud cloud) {
    this.relativeTo = relativeTo
    this.cloud = cloud

    strongUsages(relativeTo, []).each { markArgsUsed(it) }
  }

  Construction findAfter(Construction anchor, hint) {
    def c = cloud.findAfter(hint, cloud.ranges[anchor].toInt)
    return usedArgs.contains(c) ? null : c
  }


  Construction findBefore(Construction anchor, hint) {
    def cc = cloud.findBefore(hint, cloud.ranges[anchor].fromInt)
    return usedArgs.contains(cc) ? null : cc
  }

  def addConstruction(Construction c) {
    relaxed -= c
    markArgsUsed(c)
    cloud.weak.remove c
    if (cloud.ranges.containsKey(c)) return

    newConstructions << c
  }

  private List markArgsUsed(Construction c) {
    return c.args.each {
      if (it != relativeTo) {
        usedArgs += cloud.competitors[it]
      }
    }
  }

  Collection<Construction> usages(Construction c, hint) {
    return cloud.usages[c].findAll { it.isAccepted(hint, cloud) } + newConstructions.findAll { c in it.args && it.isAccepted(hint, cloud) }
  }

  Collection<Construction> strongUsages(Construction c, hint) {
    return (usages(c, hint) - relaxed).findAll { !cloud.isWeak(it) }
  }

  Construction coloredBetween(Construction from, Construction to) {
    cloud.coloredRange(cloud.ranges[from].toInt..cloud.ranges[to].fromInt)
  }

  Construction near(Construction c, boolean after, hint) {
    def range = cloud.ranges[c]
    return cloud.allAt(after ? range.toInt : range.fromInt, after).find { it.isAccepted(hint, cloud) }
  }

  Collection<List<Construction>> match(List pattern) {
    return cloud.match(relativeTo, pattern).findAll { it.intersect(usedArgs).isEmpty() }
  }

  def markFinished(Construction construction) {
    cloud.finished << construction
  }

  def relaxUsages(hint) {
    strongUsages(relativeTo, hint).each { usg ->
      Set<Construction> candidates = [] as Set
      boolean isReparseable = false
      usg.args.each {
        def comp = cloud.competitors[it]
        candidates += comp
        isReparseable |= comp.size() > 1
      }

      if (isReparseable) {
        relaxed << usg
        usedArgs -= candidates
        cloud.weak -= candidates
      }
    }
  }

  def weaken(Construction c) {
    cloud.weak << c
    demote(c)
  }

  def demote(Construction c) {
    _demoted << c
  }
}

