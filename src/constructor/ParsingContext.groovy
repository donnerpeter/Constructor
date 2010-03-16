package constructor

/**
 * @author peter
 */
class ParsingContext {
  Construction relativeTo
  Cloud cloud

  def ParsingContext(Construction relativeTo, Cloud cloud) {
    this.relativeTo = relativeTo
    this.cloud = cloud
  }

  Construction findBefore(Construction anchor, hint, serious = true) {
    def result = cloud.match(relativeTo, [hint, '_'], serious, noUsedArgs())
    return result ? result[0] : null
  }

  private IntRange compositeRange(newC) {
    int _min = Integer.MAX_VALUE
    int _max = Integer.MIN_VALUE
    newC.args.each {arg ->
      _min = Math.min(_min, cloud.ranges[arg].fromInt)
      _max = Math.max(_max, cloud.ranges[arg].toInt)
    }
    return _min.._max
  }

  def addConstruction(Construction c) {
    cloud.weak.remove c
    if (cloud.ranges.containsKey(c)) return

    cloud.addConstructions([c], compositeRange(c))
  }

  Collection<Construction> allUsages(Construction c, hint) {
    return cloud.allUsages(c, hint)
  }

  Collection<Construction> strongUsages(Construction c, hint) {
    return cloud.strongUsages(c, hint)
  }

  Construction coloredBetween(Construction from, Construction to) {
    cloud.coloredRange(cloud.ranges[from].toInt..cloud.ranges[to].fromInt)
  }

  Construction near(Construction c, boolean after, hint) {
    def range = cloud.ranges[c]
    return cloud.allAt(after ? range.toInt : range.fromInt, after).find { it.isAccepted(hint, cloud) }
  }

  private Function1<Construction, Boolean> noUsedArgs() {
    Set<Construction> usedArgs = [] as Set
    strongUsages(relativeTo, []).each { Construction usg ->
      if (usg.descr._transparent) {
        return
      }

      (usg.args - relativeTo).each {
        usedArgs += cloud.competitors[it]
      }
    }

    return { !(it in usedArgs) } as Function1
  }

  List<Construction> match(List pattern) {
    return cloud.match(relativeTo, pattern, noUsedArgs())
  }

  def markFinished(Construction construction) {
    cloud.markFinished construction
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
        cloud.weak -= candidates
        cloud.weak << usg
      }
    }
  }

  def weaken(Construction c) {
    cloud.weak << c
    demote(c)
  }

  def demote(Construction c) {
    cloud.demote(c)
  }
}

