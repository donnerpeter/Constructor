package constructor.russian

import constructor.*

/**
 * @author peter
 */
class RussianLexicon extends Lexicon {

  def RussianLexicon() {
    specials()
    preposition("в течение", "genitive") { it.expect(["_", "clause"], cons("When")) }

    noun("Власти", "nominative").aka("pl").expect(["_", "genitive"], cons("NounObj"))
    adj("московской", "genitive")
    noun("управы", "genitive")
    word('Крылатское').famous()
    verb('намерены', "pl").expect(["_", "infinitive"], cons("XComp").consumes(1))
    noun("месяца", "genitive")
    infinitive("решить").expect(["_", "accusative"], cons("Obj").consumes(1))
    noun("вопрос", "accusative")
    preposition("о", "prepositional") {it.expect(["noun": 1, "_": 0], cons("About"))}
    noun("сносе", "prepositional").aka("locatable").expect(["_", "genitive"], cons("NounObj").consumes(1))
    adj("незаконных", "genitive")
    noun("строений", "genitive").aka("locatable")
    preposition("в", "prepositional") { it.
                    expect([["noun", "locatable"]: 1, "_": 0], cons("Where")).
                    expect([["clause", "locatable"]: 1, "_": 0], cons("Where"))}
    preposition("во", ["accusative", "time"]) { it.expect(["clause": 1, "_": 0], cons("When"))}
    noun("поселке", "prepositional")
    word("Речник").famous()
    noun("Мы", "nominative").aka("1pl")
    verb("планируем", "1pl").expect(["_", "infinitive"], cons("XComp").consumes(1))
    infinitive("снести").expect(["accusative": 1, "_": 0], cons("Obj").consumes(1))
    word("все").expect(["_", "accusative"], cons("Quantifier"))
    adj("незаконные", "accusative")
    noun("постройки", "accusative")
    preposition("за", "accusative") {it.expect(["clause": 1, "_": 0], cons("When"))}
    noun("месяц", "accusative")
    verb("сообщил", "masc").aka("locatable").
            expect(["Quoted": 1, ",": 2, "-": 3, "_": 0], cons("DirectSpeech").consumes(1, 2, 3)).
            expect(["_", "dative"], cons("Goal").consumes(1))
    noun("журналистам", "dative")
    noun("вторник", "accusative").aka("time")
    noun("поселке", "prepositional")
    noun("глава", "nominative").aka("masc").expect(["_", "genitive"], cons("NounObj").consumes(1)).track()
    word("Виталий").famous().expect(["_", "Surname"], cons("NameSurname").expect(["nominative": 1, "_": 0], cons("Named")))
    word("Никитин").famous().aka("Surname")

  }

  private ConstructionBuilder infinitive(String s) {
    return word(s).famous().aka("infinitive", "clause")
  }

  private ConstructionBuilder verb(String s, String agr) {
    return word(s).aka("clause").
            expect([["nominative", agr]:1, "_":0], cons("SubjPred").consumes(1)).
            expect(["_", ["nominative", agr]], cons("SubjPred").consumes(1))
  }

  private ConstructionBuilder preposition(String s, _case, Closure binding) {
    return word(s).expect(["_", _case], binding(cons("Prepos").consumes(1)))
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
              def newargs = [this, ctx.coloredBetween(this, pair), pair]
              ctx.addConstruction(
                      new Construction("Quoted", newargs).aka("Quoted").expect(["noun":1, "_":0], cons("Appos")).consumes(0, 1, 2).demotes(0, 1, 2)
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