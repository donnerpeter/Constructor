package constructor

import constructor.russian.RussianLexicon

/**
 * @author peter
 */
class PunctuationTest extends GroovyTestCase {

  void testSimpleWord() {
    _ "в", "в"
  }

  void _testSpace() {
    _ "в в", """
Space в в
"""
  }
  
  void _test2Spaces() {
    _ "в в в", """
Space в #1=в
Space #1 в
"""
  }

  def _(input, output) {
    assertEquals output.trim(), new Parser(new RussianLexicon()).parse(input).trim()
  }

}
