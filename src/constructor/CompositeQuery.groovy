package constructor

/**
 * @author peter
 */
class CompositeQuery implements Query {
  private boolean and
  private List<Query> children = []
  private Set<Query> optional = [] as Set

  def CompositeQuery(boolean and=true) {
    this.and = and;
  }

  boolean isSatisfied(Construction c, ParsingContext ctx) {
    def patterns = obligatoryPatterns()
    def cl = { it.isSatisfied(c, ctx) }
    return and ? patterns.every(cl) : patterns.any(cl)
  }

  private List<Query> obligatoryPatterns() {
    return children - optional
  }

  def boolean matchAll(Construction c, ParsingContext ctx, Function2<SimpleQuery, List<Construction>, Void> action) {
    if (!and && isSatisfied(c, ctx)) {
      return true
    }

    def stopped = false
    def happy = true
    children.each {
      if (stopped) {
        return
      }

      def childHappy = it.matchAll(c, ctx, action)
      if (childHappy && !and) {
        stopped = true
      }

      if (!childHappy && !(it in optional)) {
        happy = false
      }
    }
    return happy
  }

  public List<SimpleQuery> listSimpleQueries() {
    def result = []
    children.each { result += it.listSimpleQueries() }
    result
  }

  CompositeQuery expect(List pattern, Descriptor action, boolean optional = false) {
    addChild(new SimpleQuery(pattern, action))
  }

  CompositeQuery expect(Map<?, Integer> pattern, Descriptor action, boolean optional = false) {
    addChild new SimpleQuery(pattern, action)
  }

  CompositeQuery evokes(Descriptor parent, int argNumber, boolean optional = false) {
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
    children.isEmpty()
  }

  CompositeQuery addChild(Query query, boolean optional = false) {
    children << query
    if (optional) {
      this.optional << query
    }
    this
  }
}
