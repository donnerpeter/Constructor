package constructor

import com.sun.jmx.mbeanserver.OpenConverter.CompositeBuilderViaConstructor.Constr

/**
 * @author peter
 */
class Construction {
  static def TAB = "  "
  String name
  List<Construction> args
  private Map<List, Closure> expectations = [:]
  private Set pings = [] as Set
  private Set<Construction> consumedArgs = [] as Set
  def famous = false
  def tracked = false

  def Construction(String name, List<Construction> args) {
    this.name = name;
    this.args = args;
    pings << name
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

  def ping(message) {
    pings.contains(message)
  }

  private def consumed(Construction c) {
    return consumedArgs.contains(c)
  }

  Construction consumes(int... argIndices) {
    argIndices.each { consumedArgs << args[it] }
    this
  }

  boolean activate(ParsingContext ctx) {
    def happy = true
    expectations.each {pattern, action ->
      def mockArgs = pattern.collect { new Construction("Mock", []) }
      def mockResult = action instanceof Closure ? action(mockArgs) : ((ConstructionBuilder)action).build(mockArgs)
      if (!ctx.usedIn(this, mockResult.pings as String[])) {
        def argLists = ctx.cloud.match(pattern)
        if (argLists) {
          argLists.each {args ->
            def result = action instanceof Closure ? action(args) : action.build(args)
            ctx.addConstruction result
          }
        } else {
          def anchor = pattern.indexOf(this)
          if (anchor > 0 && !ctx.cloud.activeBefore(ctx.cloud.ranges[pattern[anchor]].fromInt)) {
          } else {
            happy = false
          }
        }
      }
    }
    if (happy && famous && ctx.cloud.usages[this].findAll { it.consumed(this) }.isEmpty()) {
      return false
    }
    return happy
  }

  Construction aka(Object ... msg) {
    this.pings.addAll(msg as Set)
    return this
  }

  Construction expect(List pattern, action) {
    expectations[pattern.collect { it == "_" ? this : it }] = action
    return this
  }

  Construction famous() {
    famous = true
    return this
  }

  Construction track() {
    tracked = true
    return this
  }



}