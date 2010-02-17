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

  private boolean oneLiner(VarNameGenerator varName) {
    return args.every { (it.args.size() == 0 || varName.isUsage(it)) && !(it instanceof Colored) }
  }

  String prettyPrint(VarNameGenerator varName, String indent, Cloud cloud) {
    if (varName.isUsage(this)) {
      return varName.prefix(this)
    }

    def prefix = varName.prefix(this)
    String separator = oneLiner(varName) ? " " : "\n$indent$TAB"
    
    def prettyName = name.contains(' ') ? "'$name'" : name
    def a = [prettyName] + args.collect({ it.prettyPrint(varName, indent + TAB, cloud) })
    return prefix + a.join(separator)
  }

  boolean equals(o) {
    if (is(o)) return true
    if (args.empty) return false

    return o != null && o.class == this.class && o.name == name && o.args == args
  }

  int hashCode() {
    return name.hashCode() + args.hashCode();
  }

  def consumed(Construction c) {
    return consumedArgs.contains(c)
  }

  def isAccepted(hint, Cloud cloud) {
    if (hint instanceof List) {
      return hint.every { isAccepted(it, cloud) }
    } else {
      return descr.ping(hint, new ParsingContext(this, cloud))
    }
  }


}