package constructor

/**
 * @author peter
 */
class SimpleQuery implements Query {
  final List pattern
  final Descriptor descr
  final List<Integer> permutation

  def SimpleQuery(List pattern, Descriptor descr) {
    this.pattern = pattern
    this.descr = descr

    permutation = 0..<pattern.size()
  }

  def SimpleQuery(Map<?, Integer> pattern, Descriptor descr) {
    this.pattern = pattern.keySet() as List
    this.descr = descr
    permutation = new Integer[this.pattern.size()] as List

    int i = 0
    pattern.each { k, v -> permutation[i++] = v }
  }

  boolean isSatisfied(Construction c, ParsingContext ctx) {
    return !ctx.strongUsages(c, descr.name).isEmpty()
  }

  boolean matchAll(Construction c, ParsingContext ctx, Function2<SimpleQuery, List<Construction>, Void> action) {
    if (!isSatisfied(c, ctx)) {
      def matched = ctx.match(pattern)
      if (matched) {
        def args = new Construction[matched.size()] as List
        matched.eachWithIndex { arg, i -> args[permutation[i]] = arg }
        action(this, args)
        return true
      }
      return false
    }
    return true
  }

  public List<SimpleQuery> listSimpleQueries() { [this] }

  def String toString() {
    return "$pattern -> $descr.name"
  }


}