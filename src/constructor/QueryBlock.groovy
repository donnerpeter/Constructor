package constructor

import constructor.util.Pair

/**
 * @author peter
 */
class QueryBlock {
  private boolean and
  private Map<List, Descriptor> expectations = [:]
  private List<QueryBlock> children = []
  private Set<List> optional = [] as Set

  def QueryBlock(boolean and=true) {
    this.and = and;
  }

  boolean isSatisfied(Construction c, ParsingContext ctx) {
    return obligatoryPatterns().every { !ctx.strongUsages(c, expectations[it].name).isEmpty() }
  }

  private List<List> obligatoryPatterns() {
    return (expectations.keySet() as List) - optional
  }

  boolean reparse(Construction c, ParsingContext ctx) {
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

  boolean processExpectationsEagerly(Construction c, ParsingContext ctx) {
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
    children.each {
      if (!it.processExpectationsEagerly(c, ctx)) {
        happy = false
      }
    }
    return happy
  }

  QueryBlock expect(List pattern, action, boolean optional = false) {
    expectations[pattern] = action
    if (optional) {
      this.optional << pattern
    }
    return this
  }

  QueryBlock expect(Map<?, Integer> pattern, Descriptor action, boolean optional = false) {
    def delegate = new Descriptor(action.name) {

      def Construction build(List<Construction> args) {
        def permuted = new Construction[args.size()] as List
        args.eachWithIndex { arg, i ->
          permuted[pattern[(pattern.keySet() as List)[i]]] = arg
        }
        action.build(permuted)
      }

    }
    expect(pattern.keySet() as List, delegate, optional)
  }

  QueryBlock evokes(Descriptor parent, int argNumber, boolean optional = false) {
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


  def isEmpty() {
    expectations.isEmpty() && children.isEmpty()
  }

  QueryBlock childBlock(QueryBlock queryBlock) {
    children << queryBlock
    this
  }
}
