package constructor.russian

import constructor.*

/**
 * @author peter
 */
class RussianLexicon extends Lexicon {

  def RussianLexicon() {
    specials()
    storage["в течение"] = { new Word("в течение").expect(["_", "genitive"]) {
      new Construction("Prepos", it).expect(["_", "clause"]) {
        new Construction("When", it)
      }
    } }
    storage["Власти"] = { new Word("Власти").aka("nominative", "noun").expect(["_", "genitive"]) { new Construction("NounObj", it) }}
    storage["московской"] = { new Word("московской").expect(["_", "noun"]) { new Construction("AdjNoun", it) } }
    storage["управы"] = { new Word("управы").aka("noun", "genitive") }

    storage['Крылатское'] = { new Word('Крылатское') }
    storage['намерены'] = { 
      new Word('намерены').expect(["nominative", "_"]) { new Construction("SubjPred", it) }.expect(["_", "infinitive"]) { new Construction("XComp", it) }
      }

    storage['месяца'] = { new Word("месяца").aka("noun", "genitive") }
    storage['решить'] = { new Word("решить").aka("infinitive", "clause").expect(["_", "accusative"]) { new Construction("Obj", it) } }
    storage['вопрос'] = { new Word("вопрос").aka("noun", "accusative") }
    storage['о'] = { new Word("о").expect(["_", "prepositional"]) {
      new Construction("Prepos", it).expect(["noun", "_"]) { new Construction("About", [it[1], it[0]])} }
    }
    storage['сносе'] = { new Word("сносе").aka("noun", "prepositional", "locatable").expect(["_", "genitive"]) { new Construction("NounObj", it) } }
    storage['незаконных'] = { new Word("незаконных").expect(["_", "genitive"]) { new Construction("AdjNoun", it)} }
    storage['строений'] = { new Word("строений").aka("noun", "genitive", "locatable") }
    storage['в'] = { new Word("в").expect(["_", "prepositional"]) {
      new Construction("Prepos", it)
              .expect([["noun", "locatable"], "_"]) { new Construction("Where", [it[1], it[0]])}
              .expect([["clause", "locatable"], "_"]) { new Construction("Where", [it[1], it[0]])}
              }
    }
    storage['во'] = { new Word("во").expect(["_", ["accusative", "time"]]) {
      new Construction("Prepos", it).expect(["clause", "_"]) { new Construction("When", [it[1], it[0]])} }
    }
    storage['поселке'] = { new Word("поселке").aka("noun", "prepositional") }
    storage['Речник'] = { new Word("Речник") }
    storage['Мы'] = { new Word("Мы").aka("noun", "nominative", "1pl") }
    storage['планируем'] = {
      new Word("планируем").expect([["nominative", "1pl"], "_"]) { new Construction("SubjPred", it) }.expect(["_", "infinitive"]) { new Construction("XComp", it) }
    }
    storage['снести'] = { new Word("снести").aka("infinitive", "clause").expect(["accusative", "_"]) { new Construction("Obj", [it[1], it[0]]) } }
    storage['все'] = { new Word("все").expect(["_", "accusative"]) { new Construction("Quantifier", it) } }
    storage['незаконные'] = { new Word("незаконные").expect(["_", "accusative"]) { new Construction("AdjNoun", it) } }
    storage['постройки'] = { new Word("постройки").aka("noun", "accusative") }
    storage['за'] = { new Word("за").expect(["_", "accusative"]) {
      new Construction("Prepos", it).expect(["clause", "_"]) { new Construction("When", [it[1], it[0]])} }
    }
    storage['месяц'] = { new Word("месяц").aka("noun", "accusative") }
    storage['сообщил'] = {
      new Word("сообщил").aka("clause", "locatable")
              .expect(["_", "nominative"]) {
        new Construction("SubjPred", [it[1], it[0]]) }
              .expect(["Quoted", ",", "-", "_"]) { new Construction("DirectSpeech", it) }
              .expect(["_", "dative"]) { new Construction("Goal", it) }
    }
    storage['журналистам'] = { new Word("журналистам").aka("noun", "dative") }
    storage['вторник'] = { new Word("вторник").aka("noun", "accusative", "time") }
    storage['поселке'] = { new Word("поселке").aka("noun", "prepositional") }
    storage['глава'] = { new Word("глава").aka("noun", "nominative").expect(["_", "genitive"]) { new Construction("NounObj", it)} }
    storage['Виталий'] = { new Word("Виталий").expect(["_", "Surname"]) {
      new Construction("NameSurname", it).expect(["nominative", "_"]) { new Construction("Named", it) } }}
    storage['Никитин'] = { new Word("Никитин").aka("Surname") }

  }

  def specials() {
    storage['"'] = {
      new Word('"'){

        def Boolean ping(Object message) {
          message == name
        }

        def activate(ParsingContext ctx) {
          Colored colored = ctx.pushColor()
          ctx.expect([this, '"']) {
            return new Quoted([it[0], colored, it[1]])
          }
        }


      }.aka('"').expect(["_", Construction, '"']) {
        return new Quoted(it) } }

    storage[','] = { new Word(",").aka(",") }
    storage['-'] = { new Word("-").aka("-") }
  }

}

class Quoted extends Construction {

  def Quoted(args) {
    super("Quoted", args);
    famous()
  }

  def ping(Object message) {
    message == name
  }

  def Object activate(ParsingContext ctx) {
    ctx.popColor(args[2], this)
    ctx.deactivate(args[0])
    ctx.deactivate(args[1])
    ctx.deactivate(args[2])
    ctx.expect(["noun", this]) { return new Construction("Appos", it) }
  }


}