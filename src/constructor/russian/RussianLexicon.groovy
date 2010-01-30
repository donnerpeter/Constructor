package constructor.russian

import constructor.Construction
import constructor.Lexicon
import constructor.Space
import constructor.Word
import constructor.ParsingContext

/**
 * @author peter
 */
class RussianLexicon extends Lexicon {

  def RussianLexicon() {
    //storage["в течение"] = { return new Word("в течение") }
    storage["Власти"] = { return new Noun("Власти") }
    storage["московской"] = { return new Word("московской") {

      def Object activate(ParsingContext ctx) {
        ctx.expect([this, Noun]) { new Construction("AdjNoun", it) }
      }

    } }
    storage["управы"] = { return new Noun("управы") {

      def Object activate(ParsingContext ctx) {
        ctx.expect([Noun, this]) { new Construction("Posess", it) }
      }

    } }
    storage['\"'] = { return new Quote() }
    storage['Крылатское'] = { return new Word('Крылатское') }
  }

  Construction recognize(String s) {
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

class Quote extends Construction {

  def Quote() {
    super('\"', []);
  }

  def Object activate(ParsingContext ctx) {
    ctx.expect([this, Construction, Quote], { return new Quoted(it) })
  }


}

class Quoted extends Construction {

  def Quoted(args) {
    super("Quoted", args);
  }

  def Object activate(ParsingContext ctx) {
    ctx.expect([Noun, this]) { return new Construction("Appos", it) }
  }


}