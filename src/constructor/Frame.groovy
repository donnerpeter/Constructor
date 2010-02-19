package constructor

/**
 * @author peter
 */
class Frame {
  private static final TAB = Construction.TAB

  String name
  Map attributes = [:]

  def Frame(name) {
    this.name = name;
  }

  def getAt(String name) { attributes[name] }

  def putAt(String name, value) { attributes[name] = value }

  def String toString() {
    if (attributes) {
      return "$name $attributes"
    }
    return name
  }

  def prettyPrint(FrameNameGenerator gen, String indent) {
    if (gen.isUsage(this)) {
      return gen.prefix(this)
    }

    def prefix = gen.prefix(this)
    String result = prefix + name
    attributes.each { n, v ->
      result += "\n$indent|$n:"
      if (!(v instanceof Frame)) {
        result += " " + v
      }
      else if (!v.attributes) {
        result += " " + v.prettyPrint(gen, indent)
      }
      else {
        result += "\n" + indent + TAB + v.prettyPrint(gen, indent + TAB)
      }
    }

    return result
  }
}
