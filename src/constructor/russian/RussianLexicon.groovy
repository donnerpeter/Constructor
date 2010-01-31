package constructor.russian

import constructor.Construction
import constructor.Lexicon
import constructor.Word
import constructor.ParsingContext

/**
 * @author peter
 */
class RussianLexicon extends Lexicon {

  def RussianLexicon() {
    storage["в течение"] = { new Word("в течение").expect(["_", "noun"]) {
      new Construction("Prepos", it).expect(["_", "clause"]) {
        new Construction("When", it)
      }
    } }
    storage["Власти"] = { new Word("Власти").aka("nominative", "noun") }
    storage["московской"] = { new Word("московской").expect(["_", "noun"]) { new Construction("AdjNoun", it) } }
    storage["управы"] = { new Word("управы").aka("noun").expect(["noun", "_"]) { new Construction("NounGenitive", it) {

          def Object activate(ParsingContext ct) {
            ct.reactivate(args[0])
          }

        } }
      }
    storage['"'] = { new Word('"').aka('"').expect(["_", Construction, '"']) { return new Quoted(it) } }
    storage['Крылатское'] = { new Word('Крылатское') }
    storage['намерены'] = { 
      new Word('намерены').expect(["nominative", "_"]) { new Construction("SubjPred", it) }.expect(["_", "infinitive"]) { new Construction("XComp", it) }
      }

    storage['месяца'] = { new Word("месяца").aka("noun") }
    storage['решить'] = { new Word("решить").aka("infinitive", "clause") }
  }

  Construction recognize(String s) {
    return null
  }

}

class Quoted extends Construction {

  def Quoted(args) {
    super("Quoted", args);
  }

  def Object activate(ParsingContext ctx) {
    ctx.deactivate(args[0])
    ctx.deactivate(args[1])
    ctx.deactivate(args[2])
    ctx.expect(["noun", this]) { return new Construction("Appos", it) }
  }


}