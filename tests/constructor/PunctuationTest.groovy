package constructor

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
    assertEquals output.trim(), Parser.parse(input).trim()
  }

}
