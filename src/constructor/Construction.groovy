package constructor

/**
 * @author peter
 */
class Construction {
  static def TAB = "  "
  String name
  Descriptor descr
  List<Construction> args
  private Set<Construction> consumedArgs = [] as Set
  def tracked = false

  def Construction(Descriptor descr, List<Construction> args) {
    this.descr = descr
    this.args = args;
    name = descr.name;
    descr.consumedArgs.each { this.consumedArgs << args[it] }
    tracked = descr.isTracked()
  }

  def String toString() {
    if (args.isEmpty()) return name
    return "(" + ([name] + args).join(' ') + ")";
  }

  boolean oneLiner() {
    return args.every { it.args.size() == 0 && !(it instanceof Colored) }
  }

  String prettyPrint(Closure varName, String indent, Cloud cloud) {
    def prettyName = name.contains(' ') ? "'$name'" : name
    def a = [prettyName] + args.collect({arg ->
      def prefix = varName(arg)
      if (!prefix || prefix.endsWith("=")) {
        return prefix + arg.prettyPrint(varName, indent + TAB, cloud)
      }
      return prefix
    })

    String separator = oneLiner() ? " " : "\n$indent$TAB"
    return a.join(separator)
  }

  def consumed(Construction c) {
    return consumedArgs.contains(c)
  }

  def isAccepted(hint) {
    if (hint instanceof List) {
      return hint.every { isAccepted(it) }
    } else {
      return descr.ping(hint)
    }
  }


}