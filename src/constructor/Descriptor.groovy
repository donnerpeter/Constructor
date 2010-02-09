package constructor

/**
 * @author peter
 */
class Descriptor {
  def name
  private def famous = false
  private def tracked = false
  private Set<String> pings = [] as Set
  private Map<List, Descriptor> expectations = [:]
  Set<Integer> consumedArgs = [] as Set
  Set<Integer> demotedArgs = [] as Set

  def Descriptor(String name) {
    this.name = name
    pings << name
  }

  Descriptor aka(String... akas) {
    this.pings.addAll(Arrays.asList(akas))
    return this
  }

  Descriptor track() {
    tracked = true
    this
  }
  Descriptor famous() {
    famous = true
    this
  }

  Descriptor expect(List pattern, action) {
    expectations[pattern] = action
    return this
  }

  Descriptor expect(Map<?, Integer> pattern, Descriptor action) {
    def list = pattern.keySet() as List
    expectations[list] = new Descriptor(action.name) {

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

  Construction build(List<Construction> args) {
    return new Construction(this, args)
  }

  Descriptor consumes(int... argIndices) {
    consumedArgs.addAll(argIndices as List)
    return this
  }
  
  Descriptor demotes(int... argIndices) {
    demotedArgs.addAll(argIndices as List)
    return this
  }

  def ping(message) {
    pings.contains(message)
  }

  boolean activate(Construction c, ParsingContext ctx) {
    def happy = true
    expectations.each {pattern, Descriptor builder ->
      if (!ctx.usedIn(c, builder.name)) {
        def argLists = ctx.cloud.match(c, pattern)
        if (argLists) {
          argLists.each {args ->
            def result = builder.build(args)
            ctx.addConstruction result
          }
        } else {
          if (pattern.indexOf("_") == 0 || ctx.cloud.activeBefore(ctx.cloud.ranges[c].fromInt).size() > 0) {
            happy = false
          }
        }
      }
    }
    if (happy && famous && ctx.cloud.usages[this].findAll { it.consumed(c) }.isEmpty()) {
      return false
    }
    return happy
  }

  boolean shouldActivate() {
    return famous || !expectations.isEmpty()
  }


  boolean isTracked() { tracked }
}
