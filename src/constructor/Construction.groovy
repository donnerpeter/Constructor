package constructor

/**
 * @author peter
 */
class Construction {
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
    return prettyPrint({""}, "", [:]);
  }

  boolean oneLiner() {
    return args.every { it.args.size() == 0 }
  }

  String prettyPrint(Closure varName, String indent, Map<Construction, Integer> colors) {
    def prettyName = name.contains(' ') ? "'$name'" : name
    def color = colors[this]
    if (args.isEmpty() && color) {
      return "$prettyName{$color}"
    }

    def a = [prettyName] + args.collect({arg ->
      def prefix = varName(arg)
      if (!prefix || prefix.endsWith("=")) {
        return prefix + arg.prettyPrint(varName, indent + "\t", colors)
      }
      return prefix
    })

    String separator = oneLiner() ? " " : "\n$indent\t"
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