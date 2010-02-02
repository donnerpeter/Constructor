package constructor

/**
 * @author peter
 */
class ParsingContext {
  def construction
  Cloud cloud
  int pos
  Map<List, Closure> expectations = [:]
  int colorDelta = 0
  Construction[] oldColored = []

  def expect(List args, Closure action) {
    expectations[args] = action
  }

  def reactivate(Construction construction) {
    cloud.promote(construction)
  }

  def deactivate(Construction construction) {
    cloud.demote(construction)
  }

  Colored pushColor() {
    cloud.pushColor(pos)
  }
  
  def popColor(Construction... outsideConstructions) {
    oldColored = outsideConstructions
  }
}

