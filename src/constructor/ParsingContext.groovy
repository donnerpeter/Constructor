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
  
}

