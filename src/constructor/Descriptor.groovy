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
  protected Map<List, Descriptor> expectations = [:]
  protected Set<List> optional = [] as Set //too make private when Groovy fixes the bug
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
    expectations[pattern] = action
    if (optional) {
      this.optional << pattern
    }
    return this
  }

  Descriptor expect(Map<?, Integer> pattern, Descriptor action, boolean optional = false) {
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
    if (optional) {
      this.optional << list
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

  private boolean isSatisfied(Construction c, ParsingContext ctx) {
    return obligatoryPatterns().every { !ctx.strongUsages(c, expectations[it].name).isEmpty() }
  }

  private List<List> obligatoryPatterns() {
    return (expectations.keySet() as List) - optional
  }

  private boolean reparse(Construction c, ParsingContext ctx) {
    for (descr in expectations.values()) {
      ctx.relaxUsages(descr.name)
    }

    while (true) {
      Map<Set<Construction>, List<Pair<List, List<Construction>>>> cons2Patterns = [:]
      expectations.each { pattern, descr ->
        if (!ctx.strongUsages(c, descr.name)) {
          ctx.match(pattern).each { args ->
            args.each { cons2Patterns.get(ctx.cloud.competitors[it], []) << new Pair(pattern, args) }
          }
        }
      }
      def single = cons2Patterns.find { k, v -> v.size() == 1 }
      if (single) {
        def pair = single.value[0]
        ctx.addConstruction(expectations[pair.fst].build(pair.snd))
      } else {
        break
      }
    }

    return processExpectationsEagerly(c, ctx)
  }

  boolean activate(Construction c, ParsingContext ctx) {
    if (ctx.cloud.finished.contains(c) && !isSatisfied(c, ctx)) {
      ctx.cloud.finished.remove c
      return reparse(c, ctx)
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

    boolean happy = processExpectationsEagerly(c, ctx)
    if (happy && famous && ctx.usages(c, []).findAll { it.consumed(c) }.isEmpty()) {
      return false
    }
    return happy
  }

  private boolean processExpectationsEagerly(Construction c, ParsingContext ctx) {
    def happy = true
    expectations.each {pattern, Descriptor builder ->
      if (!ctx.strongUsages(c, builder.name)) {
        def argLists = ctx.match(pattern)
        if (argLists) {
          argLists.each {args ->
            ctx.addConstruction builder.build(args)
          }
        } else {
          if (pattern.indexOf("_") == 0 || ctx.cloud.activeBefore(ctx.cloud.ranges[c].fromInt).size() > 0) {
            if (!optional.contains(pattern)) {
              if (c.tracked) {
                c
              }
              happy = false
            }
          }
        }
      }
    }
    return happy
  }

  boolean shouldActivate() {
    return famous || !expectations.isEmpty()
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
    parent._patterns.each {
      def copy = it.clone()
      if (copy instanceof List) {
        copy[argNumber] = "_"
        expect(copy, parent, optional)
      } else {
        assert copy instanceof Map
        def key = copy.find { k, v -> v == argNumber }.key
        copy.remove key
        copy["_"] = argNumber
        expect(copy, parent, optional)
      }
    }
    this
  }


}
