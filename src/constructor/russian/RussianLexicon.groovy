package constructor.russian

import constructor.*

/**
 * @author peter
 */
class RussianLexicon extends Lexicon {

  def RussianLexicon() {
    specials()
    preposition("в течение", "genitive") { it.expect(["_", "clause"], cons("When")) }
    preposition("о", "prepositional") {it.expect(["noun": 1, "_": 0], cons("About"))}
    preposition("в", "prepositional") { it.
                    expect([["noun", "locatable"]: 1, "_": 0], cons("Where")).
                    expect([["clause", "locatable"]: 1, "_": 0], cons("Where"))}
    preposition("во", ["accusative", "time"]) { it.expect(["clause": 1, "_": 0], cons("When"))}
    preposition("за", "accusative") {it.expect(["clause": 1, "_": 0], cons("When"))}
    preposition("с", "genitive") {it.expect(["_", "clause"], cons("When"))}

    noun("Власти", "nominative").aka("pl").expect(["_", "genitive"], cons("NounObj"))
    adj("московской", "genitive")
    noun("управы", "genitive")
    word('Крылатское').famous()
    verb('намерены', "pl").expect(["_", "infinitive"], cons("XComp").consumes(1))
    noun("месяца", "genitive")
    infinitive("решить").expect(["_", "accusative"], cons("Obj").consumes(1))
    noun("вопрос", "accusative")
    noun("сносе", "prepositional").aka("locatable").expect(["_", "genitive"], cons("NounObj").consumes(1))
    adj("незаконных", "genitive")
    noun("строений", "genitive").aka("locatable")
    noun("поселке", "prepositional")
    word("Речник").famous()
    noun("Мы", "nominative").aka("1pl")
    verb("планируем", "1pl").expect(["_", "infinitive"], cons("XComp").consumes(1))
    infinitive("снести").expect(["accusative": 1, "_": 0], cons("Obj").consumes(1))
    word("все").expect(["_", "accusative"], cons("Quantifier"))
    adj("незаконные", "accusative")
    noun("постройки", "accusative")
    noun("месяц", "accusative")
    verb("сообщил", "masc").aka("locatable").
            expect(["Quoted": 1, ",": 2, "-": 3, "_": 0], cons("DirectSpeech").consumes(1, 2, 3)).
            expect(["_", ["dative", "animate"]], cons("Goal").consumes(1)).
            expect(["_", ",", "что", "clause"], cons("Comp").consumes(1, 2, 3).demotes(1, 2))
    noun("журналистам", "dative").aka("animate") //todo hack
    noun("вторник", "accusative").aka("time")
    noun("поселке", "prepositional")
    noun("глава", "nominative").aka("masc").expect(["_", "genitive"], cons("NounObj").consumes(1))
    word("Виталий").famous().expect(["_", "Surname"], cons("NameSurname").expect(["nominative": 1, "_": 0], cons("Named")))
    word("Никитин").famous().aka("Surname")
    noun("Он", "nominative").aka("masc")
    word("что").famous()
    noun("утра", "genitive")
    word("уже").expect(["_", "clause"], cons("Already").consumes(0))
    verb("демонтированы", "pl")
    word("два").expect(["_", "genitive"], cons("Quantity").famous().consumes(0, 1).aka("nominative", "pl"))
    noun("строения", "genitive")
    word("к").famous()
    noun("сносу", "dative")
    verb("готовится", "3sg").expect(["к":1, "dative":2, "_":0], cons("Oblique").consumes(1, 2))
    word("третий").expect(["_", "nominative"], cons("Order")).consumes(0)
    noun("дом", "nominative").aka("3sg")


  }

  private Descriptor infinitive(String s) {
    return word(s).famous().aka("infinitive", "clause")
  }

  private Descriptor verb(String s, String agr) {
    return word(s).aka("clause").
            expect([["nominative", agr]:1, "_":0], cons("SubjPred").consumes(1)).
            expect(["_", ["nominative", agr]], cons("SubjPred").consumes(1))
  }

  private Descriptor preposition(String s, _case, Closure binding) {
    return word(s).expect(["_", _case], binding(cons("Prepos").consumes(1)))
  }

  private Descriptor adj(String name, String _case) {
    return word(name).expect(["_", _case], cons("AdjNoun"))
  }

  def noun(String name, String _case) {
    return word(name).aka("noun", _case).famous()
  }

  Descriptor word(String name) {
    def cb = cons(name)
    storage[name] = cb
    return cb
  }

  Descriptor cons(String name) {
    return new Descriptor(name)
  }

  def specials() {
    storage['"'] = new Descriptor('"') {

      boolean activate(Construction c, ParsingContext ctx) {
        if (ctx.usedIn(c, "Quoted")) {
          return true
        }

        def pair = ctx.findAfter(c, '"')
        if (pair) {
          def newArgs = [c, ctx.coloredBetween(c, pair), pair]
          def descr = cons("Quoted").expect(["noun":1, "_":0], cons("Appos")).consumes(0, 1, 2).demotes(0, 1, 2)
          ctx.addConstruction(descr.build(newArgs))
          return true
        }
        return false
      }

    }.famous()

    word(',').famous()
    word('-').famous()

  }

}