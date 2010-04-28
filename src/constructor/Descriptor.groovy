package constructor

/**
 * @author peter
 */
class Descriptor {
  String name
  private def famous = false
  private def tracked = false
  private Set<String> pings = [] as Set
  protected CompositeQuery queries = new CompositeQuery(true) //todo make private when Groovy fixes the bug
  Set<Integer> consumedArgs = [] as Set
  Set<Integer> demotedArgs = [] as Set
  private int _replaces = -1
  private Map<Integer, Object> argPings = [:]
  private Closure _semantics
  List _patterns = []
  boolean _transparent = false

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

  Construction build(List<Construction> args, Cloud cloud) {
    return new Construction(this, args.collect { PhraseConstruction.project(it, cloud) })
  }

  Descriptor consumes(int... argIndices) {
    consumedArgs.addAll(argIndices as List)
    return this
  }
  
  Descriptor demotes(int... argIndices) {
    demotedArgs.addAll(argIndices as List)
    return this
  }

  boolean ping(message, ParsingContext ctx) {
    pings.contains(message)
  }

  PhraseDescriptor makePhrase(Construction c, ParsingContext ctx) {
    null
  }

  boolean activate(Construction c, ParsingContext ctx) {
    if (ctx.cloud.finished.contains(c) && !queries.isSatisfied(c, ctx)) {
      ctx.cloud.finished.remove c
      return reparse(c, ctx)
    }

    if (_replaces >= 0) {
      def victim = c.children(ctx.cloud)[0]
      ctx.strongUsages(victim, []).each { usg ->
        if (usg != c) {
          def newArgs = usg.args.collect { it == victim ? c : it }
          ctx.addConstruction usg.descr.build(newArgs, ctx.cloud)
          ctx.weaken(usg)
        }

      }
    }

    if (c.tracked) {
      c
    }

    boolean happy = processExpectationsEagerly(c, ctx)
    if (happy && famous && ctx.allUsages(c, []).findAll { it.consumed(c) }.isEmpty()) {
      return false
    }
    return happy
  }

  private boolean reparse(Construction c, ParsingContext ctx) {
    queries.listSimpleQueries().each { SimpleQuery q -> ctx.relaxUsages(q.descr.name) }

    while (true) {
      Map<List<Construction>, List<Pair<SimpleQuery, List<Construction>>>> cons2Patterns = [:]
      queries.matchAll(c, ctx, { query, args ->
        args.each { cons2Patterns.get(ctx.cloud.competitors[it], []) << new Pair(query, args) }
      } as Function2)

      def single = cons2Patterns.find { k, v -> v.size() == 1 }
      if (single) {
        Pair<SimpleQuery, List<Construction>> pair = single.value[0]
        ctx.addConstruction(pair.first.descr.build(pair.second, ctx.cloud))
      } else {
        break
      }
    }

    return processExpectationsEagerly(c, ctx)
  }

  private boolean processExpectationsEagerly(Construction c, ParsingContext ctx) {
    return queries.matchAll(c, ctx, { SimpleQuery query, args -> ctx.addConstruction query.descr.build(args, ctx.cloud) } as Function2)
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

  Descriptor variants(Query... vars) {
    def composite = new CompositeQuery(false)
    vars.each { composite.addChild(it) }
    queries.addChild(composite)
    this
  }

  Descriptor form(pattern) {
    _patterns << pattern
    this
  }

  Descriptor transparent() {
    _transparent = true
    this
  }

  Descriptor evokes(Descriptor parent, int argNumber, boolean optional = false) {
    queries.evokes(parent, argNumber, optional)
    this
  }


}
