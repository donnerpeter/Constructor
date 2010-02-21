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

  def getAt(String name) {
    def result = attributes[name]
    if (result instanceof SlotReference) {
      return result.frame[result.slot]
    }
    return result
  }

  def putAt(String name, value) { attributes[name] = value }

  def ref(String name) { new SlotReference(frame:this, slot:name) }

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
    attributes.keySet().each { n ->
      def v = this[n]
      if (!v) return

      result += "\n$indent|$n:"
      if (!(v instanceof Frame)) {
        result += " " + v
      }
      else if (!v.attributes || gen.isUsage(v)) {
        result += " " + v.prettyPrint(gen, indent)
      }
      else {
        result += "\n" + indent + TAB + v.prettyPrint(gen, indent + TAB)
      }
    }

    return result
  }
}
