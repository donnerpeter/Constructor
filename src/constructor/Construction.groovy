package constructor

/**
 * @author peter
 */
class Construction {
  String name
  List<Construction> args

  def Construction(String name, List<Construction> args) {
    this.name = name;
    this.args = args;
  }

  def String toString() {
    return prettyPrint({""}, "");
  }

  boolean oneLiner() {
    return args.every { it.args.size() == 0 }
  }

  String prettyPrint(Closure varName, String indent) {
    def prettyName = name.contains(' ') ? "'$name'" : name
    def a = [prettyName] + args.collect({ arg ->
      def prefix = varName(arg)
      if (!prefix || prefix.endsWith("=")) {
        return prefix + arg.prettyPrint(varName, indent + "  ")
      }
      return prefix 
    })

    String separator = oneLiner() ? " " : "\n$indent  "
    return a.join(separator)
  }

  def activate(ParsingContext ctx) {
    
  }

}