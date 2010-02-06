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

  boolean usedIn(Construction c, String[] hint) {
    cloud.usages[c].find { cloud.isAccepted(hint as List, it) } != null
  }

  Construction coloredBetween(Construction from, Construction to) {
    cloud.coloredRange(cloud.ranges[from].toInt..cloud.ranges[to].fromInt)
  }

}

