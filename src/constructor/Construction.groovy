package constructor

/**
 * @author peter
 */
class Construction {
  static def TAB = "  "
  String name
  List<Construction> args
  private Map<List, ConstructionBuilder> expectations = [:]
  private Set pings = [] as Set
  private Set<Construction> consumedArgs = [] as Set
  Set<Construction> demotedArgs = [] as Set
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

  Construction demotes(int... argIndices) {
    argIndices.each { demotedArgs << args[it] }
    this
  }

  boolean activate(ParsingContext ctx) {
    def happy = true
    expectations.each {pattern, ConstructionBuilder builder ->
      if (!ctx.usedIn(this, builder.name)) {
        def argLists = ctx.cloud.match(pattern)
        if (argLists) {
          argLists.each {args ->
            def result = builder.build(args)
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

  Construction aka(Object... msg) {
    this.pings.addAll(msg as Set)
    return this
  }

  Construction expect(List pattern, ConstructionBuilder action) {
    expectations[substitute(pattern)] = action
    return this
  }

  private List substitute(List pattern) {
    return pattern.collect { it == "_" ? this : it }
  }

  Construction expect(Map<?, Integer> pattern, ConstructionBuilder action) {
    def list = pattern.keySet() as List
    expectations[substitute(list)] = new ConstructionBuilder(action.name) {

      def Construction build(List<Construction> args) {
        def permuted = new Construction[args.size()] as List
        args.eachWithIndex { arg, i ->
          permuted[pattern[list[i]]] = arg
        }
        action.build(permuted)
      }

    }
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

  boolean shouldActivate() {
    return famous || !expectations.isEmpty()
  }



}