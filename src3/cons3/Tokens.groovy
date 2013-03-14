package cons3

import groovy.transform.TupleConstructor

/**
 * @author peter
 */
class Tokens {
  static int counter = 0
  private final Map<String, Token> allocated = [:]
  private final int id = counter++

  public Set<Token> getProperty(String name) {
    LinkedHashSet<Token> result = []
    for (i in 0..<name.length()) {
      String s = name[i]
      result << allocated.get(s, new Token(s + id))
    }
    return result
  }

  public Set<Token> getUnresolvedProperty(String name) {
    return getProperty(name)
  }

  @TupleConstructor
  private static class Token {
    final String name

    @Override
    String toString() {
      return name
    }


  }

}

@TupleConstructor
class TokenWrapper {
  Object wrapped

  @Override
  String toString() {
    return wrapped.toString() + "+"
  }
}