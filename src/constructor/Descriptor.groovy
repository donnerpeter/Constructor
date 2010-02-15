package constructor

import constructor.util.Pair

/**
 * @author peter
 */
class Descriptor {
  String name
  private def famous = false
  private def tracked = false
  private Set<String> pings = [] as Set
  private Map<List, Descriptor> expectations = [:]
  Set<Integer> consumedArgs = [] as Set
  Set<Integer> demotedArgs = [] as Set
  private Set<Pair<Integer, String>> suppressions = [] as Set
  private Map<Integer, Object> argPings = [:]

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

  def ping(message, ParsingContext ctx) {
    pings.contains(message)
  }

  boolean activate(Construction c, ParsingContext ctx) {
    def happy = true
    expectations.each {pattern, Descriptor builder ->
      if (!ctx.usages(c, builder.name)) {
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
    if (happy && famous && ctx.usages(c, []).findAll { it.consumed(c) }.isEmpty()) {
      return false
    }
    return happy
  }

  boolean shouldActivate() {
    return famous || !expectations.isEmpty()
  }


  boolean isTracked() { tracked }

  Descriptor suppresses(int argNumber, String relation) {
    suppressions << new Pair(argNumber, relation)
    return this
  }

  List<Construction> incompatible(Construction my, ParsingContext ctx) {
    def result = []
    suppressions.each {
      def name = it.snd
      def orphan = my.args[it.fst]
      result += ctx.usages(orphan, name)
    }
    return result
  }

  Descriptor identifyArgs(List pings) {
    pings.eachWithIndex { p, i -> argPings[i] = p }
    this
  }

  Descriptor identifyArgs(Map pings) {
    argPings.putAll(pings)
    this
  }

  def argumentPing(i) {
    argPings[i] ?: []
  }

  String toString() { "Descriptor:" + name }


}
