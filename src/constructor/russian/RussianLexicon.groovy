package constructor.russian

import constructor.*

/**
 * @author peter
 */
class RussianLexicon extends Lexicon {

  def RussianLexicon() {
    specials()
    word("в течение").expect(["_", "genitive"], cons("Prepos").expect(["_", "clause"], cons("When")))

    noun("Власти", "nominative").expect(["_", "genitive"], cons("NounObj"))
    adj("московской", "genitive")
    noun("управы", "genitive")
    word('Крылатское')
    word('намерены').expect(["nominative", "_"], cons("SubjPred")).expect(["_", "infinitive"], cons("XComp"))
    noun("месяца", "genitive")
    word("решить").aka("infinitive", "clause").expect(["_", "accusative"], cons("Obj"))
    noun("вопрос", "accusative")
    word("о").expect(["_", "prepositional"], cons("Prepos").expect(["noun", "_"]) { new Construction("About", [it[1], it[0]])})
    noun("сносе", "prepositional").aka("locatable").expect(["_", "genitive"], cons("NounObj"))
    adj("незаконных", "genitive")
    noun("строений", "genitive").aka("locatable")
    word("в").expect(["_", "prepositional"],
            cons("Prepos").
                    expect([["noun", "locatable"], "_"]) { new Construction("Where", [it[1], it[0]])}.
                    expect([["clause", "locatable"], "_"]) { new Construction("Where", [it[1], it[0]])})
    word("во").expect(["_", ["accusative", "time"]], cons("Prepos").expect(["clause", "_"]) { new Construction("When", [it[1], it[0]])})
    noun("поселке", "prepositional")
    word("Речник")
    noun("Мы", "nominative").aka("1pl")
    word("планируем").expect([["nominative", "1pl"], "_"], cons("SubjPred")).expect(["_", "infinitive"], cons("XComp"))
    word("снести").aka("infinitive", "clause").expect(["accusative", "_"]) { new Construction("Obj", [it[1], it[0]]) }
    word("все").expect(["_", "accusative"], cons("Quantifier"))
    adj("незаконные", "accusative")
    noun("постройки", "accusative")
    word("за").expect(["_", "accusative"], cons("Prepos").expect(["clause", "_"]) { new Construction("When", [it[1], it[0]])})
    noun("месяц", "accusative")
    word("сообщил").aka("clause", "locatable").expect(["_", "nominative"]) {
        new Construction("SubjPred", [it[1], it[0]])
      }.expect(["Quoted", ",", "-", "_"], cons("DirectSpeech")).expect(["_", "dative"], cons("Goal"))
    noun("журналистам", "dative")
    noun("вторник", "accusative").aka("time")
    noun("поселке", "prepositional")
    noun("глава", "nominative").expect(["_", "genitive"], cons("NounObj"))
    word("Виталий").expect(["_", "Surname"], cons("NameSurname").expect(["nominative", "_"], cons("Named")))
    word("Никитин").aka("Surname")

  }

  private ConstructionBuilder adj(String name, String _case) {
    return word(name).expect(["_", _case], cons("AdjNoun"))
  }

  def noun(String name, String _case) {
    return word(name).aka("noun", _case)
  }

  ConstructionBuilder word(String name) {
    def cb = cons(name).famous()
    storage[name] = cb
    return cb
  }

  ConstructionBuilder cons(String name) {
    return new ConstructionBuilder(name)
  }

  def specials() {
    storage['"'] = new ConstructionBuilder('"') {

      def Construction build(Object args) {
        return new Construction(name, []) {

          def activate(ParsingContext ctx) {
            Colored colored = ctx.pushColor()
            ctx.expect([this, '"']) {
              return new Quoted([it[0], colored, it[1]])
            }
          }

        }.famous().aka(name).expect(["_", Construction, '"']) {
          return new Quoted(it) } }
    }

    word(',').aka(',').famous()
    word('-').aka('-').famous()
  }

}

class Quoted extends Construction {

  def Quoted(args) {
    super("Quoted", args);
    famous().aka(name)
    track()
  }

  def Object activate(ParsingContext ctx) {
    ctx.popColor(args[2], this)
    ctx.deactivate(args[0])
    ctx.deactivate(args[1])
    ctx.deactivate(args[2])
    ctx.expect(["noun", this]) { return new Construction("Appos", it) }
  }


}