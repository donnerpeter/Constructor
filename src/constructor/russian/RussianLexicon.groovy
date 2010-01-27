package constructor.russian

import constructor.Construction
import constructor.Lexicon
import constructor.Word

/**
 * @author peter
 */
class RussianLexicon extends Lexicon {

  Construction recognize(String s, Construction last) {
    if ((s.endsWith("ое")
            || s.endsWith("ой")
            || s.endsWith("ых")
            || s.endsWith("ть")
            || s.endsWith("ий")
    ) && s.size() > 3) {
      return new Inflection("-${s[-2..-1]}", stem(s[0..-3]))
    }
    if ((s.endsWith("и") ||
            s.endsWith("ы") ||
            s.endsWith("а") ||
            s.endsWith("е")) && s.size() > 2) {
      return new Inflection("-${s[-1..-1]}", stem(s[0..-2]))
    }

    return null
  }

  def stem(s) {
    if ("месяц" == s
            || "вопрос" == s
            || "снос" == s
    ) {
      return new Word(s)
    }
    return new Word("$s-")
  }

}
