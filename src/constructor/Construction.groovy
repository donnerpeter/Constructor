package constructor

/**
 * @author peter
 */
class Construction {
  String name
  List<Construction> args
  private Map<List, Closure> expectations = [:]
  private Set pings = [] as Set

  def Construction(String name, List<Construction> args) {
    this.name = name;
    this.args = args;
  }

  def Construction(name, args, expectations, pings) {
    this.name = name;
    this.args = args;
    this.expectations = expectations;
    this.pings = pings;
  }

  def String toString() {
    return prettyPrint({""}, "");
  }

  boolean oneLiner() {
    return args.every { it.args.size() == 0 }
  }

  String prettyPrint(Closure varName, String indent) {
    def prettyName = name.contains(' ') ? "'$name'" : name
    def a = [prettyName] + args.collect({arg ->
      def prefix = varName(arg)
      if (!prefix || prefix.endsWith("=")) {
        return prefix + arg.prettyPrint(varName, indent + "  ")
      }
      return prefix
    })

    String separator = oneLiner() ? " " : "\n$indent  "
    return a.join(separator)
  }

  def ping(message) {
    pings.contains(message)
  }

  def activate(ParsingContext ctx) {
    expectations.each { pattern, action -> ctx.expect(pattern, action) }
  }

  Construction aka(Object ... msg) {
    Set pings = msg as Set
    pings.addAll(this.pings)
    return new Construction(name, args, expectations, pings)
  }

  Construction expect(List pattern, Closure action) {
    for (i in 0..pattern.size()-1) {
      if (pattern[i] == "_") pattern[i] = this
    }
    Map expectations = this.expectations.clone()
    expectations[pattern] = action
    def result = new Construction(name, args, expectations, pings)
    result.expectations.each { k, v ->
      for (i in 0..k.size()-1) {
        if (k[i] == this) k[i] = result
      }
    }
    return result
  }

}