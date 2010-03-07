package constructor

/**
 * @author peter
 */
class Descriptor {
  String name
  private def famous = false
  private def tracked = false
  private Set<String> pings = [] as Set
  protected QueryBlock queries = new QueryBlock(true) //too make private when Groovy fixes the bug
  Set<Integer> consumedArgs = [] as Set
  Set<Integer> demotedArgs = [] as Set
  private int _replaces = -1
  private Map<Integer, Object> argPings = [:]
  private Closure _semantics
  List _patterns = []

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

  Descriptor expect(List pattern, action, boolean optional = false) {
    queries.expect(pattern, action, optional)
    return this
  }

  Descriptor expect(Map<?, Integer> pattern, Descriptor action, boolean optional = false) {
    queries.expect(pattern, action, optional)
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
    if (ctx.cloud.finished.contains(c) && !queries.isSatisfied(c, ctx)) {
      ctx.cloud.finished.remove c
      return queries.reparse(c, ctx)
    }

    if (_replaces >= 0) {
      def victim = c.children(ctx.cloud)[0]
      ctx.strongUsages(victim, []).each { usg ->
        if (usg != c) {
          def newArgs = usg.args.collect { it == victim ? c : it }
          ctx.addConstruction usg.descr.build(newArgs)
          ctx.weaken(usg)
        }

      }
    }

    demotedArgs.each { ctx.demote(c.args[it]) }

    boolean happy = queries.processExpectationsEagerly(c, ctx)
    if (happy && famous && ctx.usages(c, []).findAll { it.consumed(c) }.isEmpty()) {
      return false
    }
    return happy
  }

  boolean shouldActivate() {
    return famous || !queries.isEmpty()
  }

  boolean isTracked() { tracked }

  Descriptor replaces(int argNumber) {
    _replaces = argNumber
    demotes(argNumber)
    return this
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

  def buildSemantics(args) {
    if (!_semantics) {
      return null
    }

    return _semantics.call(args)
  }

  Descriptor semantics(Closure c) {
    _semantics = c
    this
  }

  Descriptor frame(String id) {
    _semantics = { new Frame(id) }
    this
  }

  Descriptor form(pattern) {
    _patterns << pattern
    this
  }

  Descriptor evokes(Descriptor parent, int argNumber, boolean optional = false) {
    queries.evokes(parent, argNumber, optional)
    this
  }


}
