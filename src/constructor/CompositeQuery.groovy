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
    return obligatoryPatterns().every { it.isSatisfied(c, ctx) }
  }

  private List<Query> obligatoryPatterns() {
    return children - optional
  }

  def boolean matchAll(Construction c, ParsingContext ctx, Function2<SimpleQuery, List<Construction>, Void> action) {
    def happy = true
    children.each {
      if (!it.matchAll(c, ctx, action) && !(it in optional)) {
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
    children << new SimpleQuery(pattern, action)
    if (optional) {
      this.optional << pattern
    }
    return this
  }

  CompositeQuery expect(Map<?, Integer> pattern, Descriptor action, boolean optional = false) {
    children << new SimpleQuery(pattern, action)
    if (optional) {
      this.optional << pattern
    }
    return this
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

  CompositeQuery childBlock(CompositeQuery queryBlock) {
    children << queryBlock
    this
  }
}
