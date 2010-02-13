package constructor

/**
 * @author peter
 */
class ParsingContext {
  Construction relativeTo
  Cloud cloud
  List<Construction> newConstructions = []
  private Set<Construction> usedArgs = [] as Set

  def ParsingContext(Construction relativeTo, Cloud cloud) {
    this.relativeTo = relativeTo;
    this.cloud = cloud;
  }

  Construction findAfter(Construction anchor, hint) {
    def c = cloud.findAfter(hint, cloud.ranges[anchor].toInt)
    return usedArgs.contains(c) ? null : c
  }

  List<Construction> findAll(Construction anchor, hint) {
    def result = []
    def c = findBefore(anchor, hint)
    if (c) result << c
    c = findAfter(anchor, hint)
    if (c) result << c
    return result
  }

  Construction findBefore(Construction anchor, hint) {
    def cc = cloud.findBefore(hint, cloud.ranges[anchor].fromInt)
    return usedArgs.contains(cc) ? null : cc
  }

  def addConstruction(Construction c) {
    usedArgs += (c.args - relativeTo)
    cloud.weak.remove c
    if (cloud.ranges.containsKey(c)) return

    newConstructions << c
  }

  Collection<Construction> usages(Construction c, hint) {
    return cloud.usages[c].findAll { it.isAccepted(hint, cloud) }
  }

  Collection<Construction> strongUsages(Construction c, hint) {
    return usages(c, hint).findAll { !cloud.weak.contains(it) }
  }

  Construction coloredBetween(Construction from, Construction to) {
    cloud.coloredRange(cloud.ranges[from].toInt..cloud.ranges[to].fromInt)
  }

  Construction near(Construction c, boolean after, hint) {
    def range = cloud.ranges[c]
    return cloud.allAt(after ? range.toInt : range.fromInt, after).find { it.isAccepted(hint, cloud) }
  }

  def markFinished(Construction construction) {
    cloud.finished << construction
  }
}

