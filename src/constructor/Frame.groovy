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

  private def stringify(v, FramePrinter gen, String indent) {
    if (v instanceof List) {
      def topLevel = v.findAll { !(it instanceof Frame) || it in gen.coloredRoots }
      return topLevel.collect { stringify(it, gen, indent)}.join("\n")
    }
    if (!(v instanceof Frame)) {
      return v.toString()
    }
    return v.prettyPrint(gen, indent)
  }

  def prettyPrint(FramePrinter gen, String indent) {
    if (gen.isUsage(this)) {
      return gen.prefix(this)
    }

    def prefix = gen.prefix(this)
    String result = prefix + name
    attributes.keySet().each { n ->
      def v = this[n]
      if (!v) return

      result += "\n$indent|$n:"
      def arg = stringify(v, gen, indent + TAB)
      if (arg.trim().contains("\n")) {
        result += "\n" + indent + TAB + arg
      } else {
        result += " " + arg
      }
    }

    return result
  }

  def allChildren() {
    def result = [] as Set
    attributes.keySet().each { n ->
      def v = this[n]
      if (!v) return

      if (v instanceof Frame) {
        result << v
        result += v.allChildren()
      } else if (v instanceof List) {
        v.each {
          if (it instanceof Frame) {
            result << it
            result += it.allChildren()
          }
        }
      }
    }
    result
  }
}
