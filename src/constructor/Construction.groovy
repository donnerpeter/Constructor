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

  List<Construction> children(Cloud cloud) { args }

  def String toString() {
    if (args.isEmpty()) return name
    return "(" + ([name] + args).join(' ') + ")";
  }

  protected boolean oneWord(Cloud cloud) {
    return args.size() == 0
  }

  private boolean oneLiner(VarNameGenerator varName, Cloud cloud) {
    return args.every { varName.isUsage(it) || it.oneWord(cloud) }
  }

  String prettyPrint(VarNameGenerator varName, String indent, Cloud cloud) {
    if (varName.isUsage(this)) {
      return varName.prefix(this)
    }

    def prefix = varName.prefix(this)
    return prefix + _prettyPrint(varName, indent, cloud)
  }

  protected String _prettyPrint(VarNameGenerator varName, String indent, Cloud cloud) {
    String separator = oneLiner(varName, cloud) ? " " : "\n$indent$TAB"

    def prettyName = name.contains(' ') ? "'$name'" : name
    def a = [prettyName] + args.collect({ it.prettyPrint(varName, indent + TAB, cloud) })
    return a.join(separator)
  }

  boolean equals(o) {
    if (is(o)) return true
    if (args.empty) return false

    return o != null && o.class == this.class && o.name == name && o.args == args
  }

  int hashCode() {
    if (args.empty) {
      return super.hashCode()
    }

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