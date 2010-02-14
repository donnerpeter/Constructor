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
    input = input.replace('\n', ' ') + " "
    Cloud cloud = new Cloud()
    int pos = 0
    while (pos < input.size()) {
      def suitable = lexicon.storage.keySet().sort({s1, s2 -> s2.size() - s1.size()}).findAll {input.substring(pos).startsWith(it) }
      if (suitable) {
        def best = suitable[0]
        def builder = lexicon.storage[best]
        cloud.addConstruction(builder.build([]), pos..pos + best.size())
        pos += best.size()
      } else {
        def space = input.indexOf(' ', pos)
        if (space > pos) {
          def word = input[pos..space-1]
          cloud.addConstruction(new Descriptor(word).build([]), pos..pos+word.size())
        }
        cloud.addConstruction(makeSpace(), space..space+1)
        pos = space+1
      }
    }

    return cloud.prettyPrint()

  }

  private Construction makeSpace() {
    return new Descriptor("Space").build([])
  }

}