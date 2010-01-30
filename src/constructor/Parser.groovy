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
    input += " "
    Cloud cloud = new Cloud()
    int pos = 0
    while (pos < input.size()) {
      def suitable = lexicon.storage.keySet().sort({s1, s2 -> s2.size() - s1.size()}).findAll {input.substring(pos).startsWith(it) }
      if (suitable) {
        def best = suitable[0]
        Construction c = lexicon.storage[best]()
        cloud.addConstruction(c, pos)
        pos += best.size()
      } else {
        def space = input.indexOf(' ', pos)
        if (space == pos) {
          pos++
          continue
        }
        def word = input[pos..space-1]
        def c = lexicon.recognize(word)
        if (c) {
          cloud.addConstruction(c, pos)
        }
        else {
          cloud.addConstruction(new Word(word), pos)
        }
        pos = space+1
      }
    }

    return cloud.prettyPrint()

  }

}