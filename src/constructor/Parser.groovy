package constructor

/**
 * @author peter
 */
class Parser {

  static String parse(String input) {
    Cloud cloud = new Cloud()
    def prevWord = null
    StringTokenizer tokenizer = new StringTokenizer(input)
    while (tokenizer.hasMoreTokens()) {
      def pos = tokenizer.currentPosition
      def t = tokenizer.nextToken()
      if (!t.trim().isEmpty()) {
        def w = new Word(t)
        cloud.addConstruction(w, pos)
        if (prevWord) {
       //   cloud.addConstruction(new Space(prevWord, w), cloud.starts[prevWord])
        }
        prevWord = w
      }
    }

    return cloud.prettyPrint()

  }

}
