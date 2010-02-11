package constructor

/**
 * @author peter
 */
class ParsingContext {
  Cloud cloud
  List<Construction> newConstructions = []

  def deactivate(Construction construction) {
    cloud.demote(construction)
  }

  Construction findAfter(Construction anchor, hint) {
    cloud.findAfter(hint, cloud.ranges[anchor].toInt)
  }

  Construction findBefore(Construction anchor, hint) {
    cloud.findBefore(hint, cloud.ranges[anchor].fromInt)
  }

  def addConstruction(Construction c) {
    newConstructions << c
  }

  Collection<Construction> usages(Construction c, hint) {
    return cloud.usages[c].findAll { it.isAccepted(hint) }
  }

  Construction coloredBetween(Construction from, Construction to) {
    cloud.coloredRange(cloud.ranges[from].toInt..cloud.ranges[to].fromInt)
  }

  Construction near(Construction c, boolean after, hint) {
    def range = cloud.ranges[c]
    return cloud.allAt(after ? range.toInt : range.fromInt, after).find { it.isAccepted(hint) }
  }
}

