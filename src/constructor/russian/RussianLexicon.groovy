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
    storage["в течение"] = { return new Word("в течение") {

      def Object activate(ParsingContext ctx) {
        ctx.expect([this, Noun]) { new Construction("Prepos", it) {

          def Object activate(ParsingContext ct) {
            ct.expect([this, "clause"]) {
              new Construction("When", it)
              }
          }

        } }
      }

    } }
    storage["Власти"] = { return new Noun("Власти") {

      def Object ping(Object message) {
        if (message == "nominative") {
          return true
        }
        return super.ping(message);
      }

    } }
    storage["московской"] = { return new Word("московской") {
                                                                                        
      def Object activate(ParsingContext ctx) {
        ctx.expect([this, Noun]) { new Construction("AdjNoun", it) }
      }

    } }
    storage["управы"] = { return new Noun("управы") {

      def Object activate(ParsingContext ctx) {
        ctx.expect([Noun, this]) { new Construction("NounGenitive", it) {

          def Object activate(ParsingContext ct) {
            ct.reactivate(args[0])
          }

        } }
      }

    } }
    storage['\"'] = { return new Quote() }
    storage['Крылатское'] = { return new Word('Крылатское') }
    storage['намерены'] = { 
      return new Word('намерены') {

      def Object activate(ParsingContext ctx) {
        ctx.expect(["nominative", this]) { new Construction("SubjPred", it) }
        ctx.expect([this, "infinitive"]) { new Construction("XComp", it) }
      }

    } }

    storage['месяца'] = { new Noun("месяца") }
    storage['решить'] = { new Verb("решить") {

      def Object ping(Object message) {
        if (message == "infinitive" || message == "clause") {
          return true
        }
        return super.ping(message);
      }

    } }
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

  def Object ping(Object message) {
    return super.ping(message);
  }

  def Object activate(ParsingContext ctx) {
    ctx.deactivate(args[0])
    ctx.deactivate(args[1])
    ctx.deactivate(args[2])
    ctx.expect([Noun, this]) { return new Construction("Appos", it) }
  }


}

class Verb extends Construction {

  def Verb(name) {
    super(name, [])
  }

}