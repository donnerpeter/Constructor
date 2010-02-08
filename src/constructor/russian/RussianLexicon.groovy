package constructor.russian

import constructor.*

/**
 * @author peter
 */
class RussianLexicon extends Lexicon {

  def RussianLexicon() {
    specials()
    preposition("в течение", "genitive") { it.expect(["_", "clause"], cons("When")) }

    noun("Власти", "nominative").expect(["_", "genitive"], cons("NounObj"))
    adj("московской", "genitive")
    noun("управы", "genitive")
    word('Крылатское').famous()
    word('намерены').expect(["nominative": 1, "_": 0], cons("SubjPred")).expect(["_", "infinitive"], cons("XComp"))
    noun("месяца", "genitive")
    infinitive("решить").expect(["_", "accusative"], cons("Obj"))
    noun("вопрос", "accusative")
    preposition("о", "prepositional") {it.expect(["noun": 1, "_": 0], cons("About"))}
    noun("сносе", "prepositional").aka("locatable").expect(["_", "genitive"], cons("NounObj"))
    adj("незаконных", "genitive")
    noun("строений", "genitive").aka("locatable")
    preposition("в", "prepositional") { it.
                    expect([["noun", "locatable"]: 1, "_": 0], cons("Where")).
                    expect([["clause", "locatable"]: 1, "_": 0], cons("Where"))}
    preposition("во", ["accusative", "time"]) { it.expect(["clause": 1, "_": 0], cons("When"))}
    noun("поселке", "prepositional")
    word("Речник").famous()
    noun("Мы", "nominative").aka("1pl")
    word("планируем").expect([["nominative", "1pl"], "_"], cons("SubjPred").consumes(0)).expect(["_", "infinitive"], cons("XComp").consumes(1))
    infinitive("снести").expect(["accusative": 1, "_": 0], cons("Obj").consumes(1))
    word("все").expect(["_", "accusative"], cons("Quantifier"))
    adj("незаконные", "accusative")
    noun("постройки", "accusative")
    preposition("за", "accusative") {it.expect(["clause": 1, "_": 0], cons("When"))}
    noun("месяц", "accusative")
    word("сообщил").aka("clause", "locatable").
            expect(["_", "nominative"], cons("SubjPred")).
            expect(["Quoted": 1, ",": 2, "-": 3, "_": 0], cons("DirectSpeech").consumes(0, 1, 2)).
            expect(["_", "dative"], cons("Goal").consumes(1))
    noun("журналистам", "dative")
    noun("вторник", "accusative").aka("time")
    noun("поселке", "prepositional")
    noun("глава", "nominative").expect(["_", "genitive"], cons("NounObj"))
    word("Виталий").famous().expect(["_", "Surname"], cons("NameSurname").expect(["nominative": 1, "_": 0], cons("Named")))
    word("Никитин").famous().aka("Surname")

  }

  private ConstructionBuilder infinitive(String s) {
    return word(s).famous().aka("infinitive", "clause")
  }

  private ConstructionBuilder preposition(String s, _case, Closure binding) {
    return word(s).expect(["_", _case], binding(cons("Prepos")))
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
                      new Construction("Quoted", [this, ctx.coloredBetween(this, pair), pair]).aka("Quoted").expect(["noun":1, "_":0], cons("Appos")).consumes(0, 1, 2)
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