package constructor.russian

import constructor.*

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
    storage["Власти"] = { new Word("Власти").aka("nominative", "noun")/*.expect(["_", "genitive"]) { new Construction("NounObj", it) }*/}
    storage["московской"] = { new Word("московской").expect(["_", "noun"]) { new Construction("AdjNoun", it).infamous() } }
    storage["управы"] = { new Word("управы").aka("noun", "genitive").expect(["noun", "_"]) { new Construction("NounGenitive", it) {

          def Object activate(ParsingContext ct) {
            ct.reactivate(args[0])
          }

        } }
      }
    storage['"'] = {
      new Word('"'){

        def Boolean ping(Object message) {
          if (message == name) {
            return true
          }
          return super.ping(message);
        }

        def activate(ParsingContext ctx) {
          Colored colored = ctx.pushColor()
          ctx.expect([this, '"']) {
            return new Quoted([it[0], colored, it[1]])
          }
        }


      }.aka('"').expect(["_", Construction, '"']) {
        return new Quoted(it) } }
    storage['Крылатское'] = { new Word('Крылатское') }
    storage['намерены'] = { 
      new Word('намерены').expect(["nominative", "_"]) { new Construction("SubjPred", it) }.expect(["_", "infinitive"]) { new Construction("XComp", it) }
      }

    storage['месяца'] = { new Word("месяца").aka("noun") }
    storage['решить'] = { new Word("решить").aka("infinitive", "clause").expect(["_", "accusative"]) { new Construction("Obj", it) } }
    storage['вопрос'] = { new Word("вопрос").aka("noun", "accusative") }
    storage['о'] = { new Word("о").expect(["_", "prepositional"]) {
      new Construction("Prepos", it).expect(["noun", "_"]) { new Construction("About", [it[1], it[0]])} }
    }
    storage['сносе'] = { new Word("сносе").aka("noun", "prepositional").expect(["_", "genitive"]) { new Construction("NounObj", it).infamous() } }
    storage['незаконных'] = { new Word("незаконных").expect(["_", "genitive"]) { new Construction("AdjNoun", it).infamous()} }
    storage['строений'] = { new Word("строений").aka("noun", "genitive") }
    storage['в'] = { new Word("в").expect(["_", "prepositional"]) {
      new Construction("Prepos", it).expect(["noun", "_"]) { new Construction("Where", [it[1], it[0]]).infamous()} }
    }
    storage['поселке'] = { new Word("поселке").aka("noun", "prepositional") }
    storage['Речник'] = { new Word("Речник") }
  }

}

class Quoted extends Construction {

  def Quoted(args) {
    super("Quoted", args);
  }

  def Object activate(ParsingContext ctx) {
    ctx.popColor(args[2], this)
    ctx.deactivate(args[0])
    ctx.deactivate(args[1])
    ctx.deactivate(args[2])
    ctx.expect(["noun", this]) { return new Construction("Appos", it) }
  }


}