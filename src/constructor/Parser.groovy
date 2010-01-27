package constructor

/**
 * @author peter
 */
class Parser {
  Lexicon lexicon


  def Parser(lexicon) {
    this.lexicon = lexicon;
  }

  String parse(String input) {
    Cloud cloud = new Cloud()
    StringTokenizer tokenizer = new StringTokenizer(input)
    while (tokenizer.hasMoreTokens()) {
      def pos = tokenizer.currentPosition
      def t = tokenizer.nextToken()
      if (!t.trim().isEmpty()) {
        def c = lexicon.recognize(t, null)
        if (c) {
          cloud.addConstruction(c, pos)
        }
        else {
          def w = new Word(t)
          cloud.addConstruction(w, pos)
        }

      }
    }

    return cloud.prettyPrint()

  }

}
