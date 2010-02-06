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
    word('Крылатское').famous()
    word('намерены').expect(["nominative", "_"], cons("SubjPred")).expect(["_", "infinitive"], cons("XComp"))
    noun("месяца", "genitive")
    word("решить").famous().aka("infinitive", "clause").expect(["_", "accusative"], cons("Obj"))
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
    word("Речник").famous()
    noun("Мы", "nominative").aka("1pl")
    word("планируем").expect([["nominative", "1pl"], "_"], cons("SubjPred").consumes(0)).expect(["_", "infinitive"], cons("XComp").consumes(1))
    word("снести").famous().aka("infinitive", "clause").expect(["accusative", "_"]) { new Construction("Obj", [it[1], it[0]]).consumes(1) }
    word("все").expect(["_", "accusative"], cons("Quantifier"))
    adj("незаконные", "accusative")
    noun("постройки", "accusative")
    word("за").expect(["_", "accusative"], cons("Prepos").expect(["clause", "_"]) { new Construction("When", [it[1], it[0]])})
    noun("месяц", "accusative")
    word("сообщил").aka("clause", "locatable").expect(["_", "nominative"]) {
        new Construction("SubjPred", [it[1], it[0]])
      }.expect(["Quoted", ",", "-", "_"], cons("DirectSpeech").consumes(0, 1, 2)).expect(["_", "dative"], cons("Goal").consumes(1))
    noun("журналистам", "dative")
    noun("вторник", "accusative").aka("time")
    noun("поселке", "prepositional")
    noun("глава", "nominative").expect(["_", "genitive"], cons("NounObj"))
    word("Виталий").famous().expect(["_", "Surname"], cons("NameSurname").expect(["nominative", "_"], cons("Named")))
    word("Никитин").famous().aka("Surname")

  }

  private ConstructionBuilder adj(String name, String _case) {
    return word(name).expect(["_", _case], cons("AdjNoun"))
  }

  def noun(String name, String _case) {
    return word(name).aka("noun", _case).famous()
  }

  ConstructionBuilder word(String name) {
    def cb = cons(name)
    storage[name] = cb
    return cb
  }

  ConstructionBuilder cons(String name) {
    return new ConstructionBuilder(name)
  }

  def specials() {
    storage['"'] = new ConstructionBuilder('"') {

      Construction build(List<Construction> args) {
        return new Construction(name, []) {

          boolean activate(ParsingContext ctx) {
            if (ctx.usedIn(this, "Quoted")) {
              return true
            }

            def pair = ctx.findAfter(this, '"')
            if (pair) {
              ctx.addConstruction(
                      new Construction("Quoted", [this, ctx.coloredBetween(this, pair), pair]).aka("Quoted").expect(["noun", "_"], cons("Appos")).consumes(0, 1, 2)
              )
              return true
            }
            return false
          }

        }.famous() }
    }

    word(',').famous()
    word('-').famous()

  }

}