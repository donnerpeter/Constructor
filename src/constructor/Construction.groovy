package constructor

/**
 * @author peter
 */
class Construction {
  static def TAB = "  "
  String name
  List<Construction> args
  private Map<List, Closure> expectations = [:]
  private Set pings = [] as Set
  private deactivate = false

  def Construction(String name, List<Construction> args) {
    this.name = name;
    this.args = args;
  }

  def String toString() {
    return "($name $args)";
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

  def ping(message) {
    pings.contains(message)
  }

  def activate(ParsingContext ctx) {
    if (deactivate) {
      ctx.deactivate(this)
    }
    expectations.each {pattern, action -> ctx.expect(pattern, action) }
  }

  Construction aka(Object ... msg) {
    this.pings.addAll(msg as Set)
    return this
  }

  Construction expect(List pattern, Closure action) {
    for (i in 0..pattern.size()-1) {
      if (pattern[i] == "_") pattern[i] = this
    }
    expectations[pattern] = action
    return this
  }

  Construction infamous() {
    deactivate = true
    return this
  }

}