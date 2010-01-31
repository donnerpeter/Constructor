package constructor

/**
 * @author peter
 */
class ParsingContext {
  def construction
  Cloud cloud
  int pos

  def expect(List args, Closure action) {
    cloud.match(args, action)
  }

  def reactivate(Construction construction) {
    cloud.promote(construction)
  }

  def deactivate(Construction construction) {
    cloud.demote(construction)
  }
}

