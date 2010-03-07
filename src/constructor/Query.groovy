package constructor

/**
 * @author peter
 */
interface Query {
  boolean isSatisfied(Construction c, ParsingContext ctx)
  boolean matchAll(Construction c, ParsingContext ctx, Function2<SimpleQuery, List<Construction>, Void> action)

  List<SimpleQuery> flatten()
}
