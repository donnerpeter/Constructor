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
                    expect([["noun", "locatable"]: 1, "_": 0], cons("Where").consumes(1)).
                    expect([["clause", "locatable"]: 1, "_": 0], cons("Where").consumes(1)).
                    expect(["_", ["clause", "locatable"]], cons("Where").consumes(1))
                    }
    preposition("во", ["accusative", "time"]) { it.expect(["clause": 1, "_": 0], cons("When"))}
    preposition("за", "accusative") {it.expect(["clause": 1, "_": 0], cons("When"))}
    preposition("с", "genitive") {it.expect(["_", "clause"], cons("When"))}

    noun("Власти", "nominative").aka("pl").expect(["_", "genitive"], cons("NounObj"))
    noun("управы", "genitive").aka("nameable")
    noun("месяца", "genitive")
    noun("вопрос", "accusative")
    noun("сносе", "prepositional").aka("locatable").expect(["_", "genitive"], cons("NounObj").consumes(1))
    noun("строений", "genitive").aka("locatable")
    noun("поселке", "prepositional").aka("nameable")
    noun("постройки", "accusative")
    noun("месяц", "accusative")
    noun("журналистам", "dative").aka("animate") //todo hack
    noun("вторник", "accusative").aka("time")
    noun("глава", "nominative").aka("masc").expect(["_", "genitive"], cons("NounObj").consumes(1))
    noun("утра", "genitive")
    noun("строения", "genitive")
    noun("сносу", "dative")
    noun("дом", "nominative").aka("3sg")
    noun("дома", "accusative").aka("genitive") //todo hack
    noun("приставы", "nominative").aka("3pl")
    noun("жители", "nominative").aka("pl")
    noun("чиновник", "nominative").aka("masc")
    noun("добро", "accusative")

    noun("Мы", "nominative").aka("1pl")
    noun("мы", "nominative").aka("1pl")
    noun("Он", "nominative").aka("masc")
    noun("нам", "dative")

    adj("московской", "genitive")
    adj("незаконных", "genitive")
    adj("незаконные", "accusative")
    adj("местные", "nominative")

    verb('намерены', "pl").expect(["_", "infinitive"], cons("XComp").consumes(1))
    verb("планируем", "1pl").expect(["_", "infinitive"], cons("XComp").consumes(1))
    verb("сообщил", "masc").aka("locatable").
            expect(["Quoted": 1, ",": 2, "-": 3, "_": 0], cons("DirectSpeech").consumes(1, 2, 3)).
            expect(["_", ["dative", "animate"]], cons("Goal").consumes(1)).
            expect(["_", ",", "что", "clause"], cons("Comp").consumes(1, 2, 3).demotes(1, 2))
    verb("сказал", "masc").expect(["Quoted": 1, ",": 2, "-": 3, "_": 0], cons("DirectSpeech").consumes(1, 2, 3))
    verb("демонтированы", "pl")
    verb("готовится", "3sg").expect(["к":1, "dative":2, "_":0], cons("Oblique").consumes(1, 2))
    verb('дают', "3pl").
            expect(["_", "добро"], cons("дать добро").consumes(1).suppresses(0, "Obj")).
            expect(["dative":1, "_":0], cons("Goal").consumes(1)).
            expect(["_", "accusative"], cons("Obj").consumes(1))
    verb("сносим", "1pl").expect(["accusative":1, "_":0], cons("Obj").consumes(1))
    verb("снесли", "1pl").expect(["не", "_", "ни", "одного", "genitive"], cons("NegObj").consumes(0, 2, 3, 4))
    verb("проживали", "pl").aka("locatable")

    infinitive("решить").expect(["_", "accusative"], cons("Obj").consumes(1))
    infinitive("снести").expect(["accusative": 1, "_": 0], cons("Obj").consumes(1))

    word('Крылатское').famous()
    word("Речник").famous()
    word("одного").famous()
    word("не").famous()
    word("ни").famous()
    word("все").expect(["_", "accusative"], cons("Quantifier"))
    word("Виталий").famous().expect(["_", "Surname"], cons("NameSurname").expect(["nominative": 1, "_": 0], cons("Named")))
    word("Никитин").famous().aka("Surname")
    word("что").famous()
    word("уже").expect(["_", "clause"], cons("Already").consumes(0))
    word("два").expect(["_", "genitive"], cons("Quantity").famous().consumes(0, 1).aka("nominative", "pl"))
    word("к").famous()
    word("третий").expect(["_", "nominative"], cons("Order").consumes(0))
    word("Когда").expect(["_", "clause"], cons("XWhen").consumes(0).expect(["_", ",", "clause"], cons("When").consumes(1)))
    word("эти").expect(["_", "accusative"], cons("Demonstrative"))
    word("котором").aka("prepositional").expect(["noun":1, ",":2, "_":0], cons("Relative"))
    word("бы").expect(["_", "clause"], cons("Subjunctive"))

  }

  private Descriptor infinitive(String s) {
    return word(s).famous().aka("infinitive", "clause")
  }

  private Descriptor verb(String s, String agr) {
    return verb(word(s), agr)
  }

  def verb(Descriptor descriptor, String agr) {
    return descriptor.aka("clause").
            expect([["nominative", agr]: 1, "_": 0], cons("SubjPred").consumes(1)).
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
        if (ctx.usages(c, "Quoted")) {
          return true
        }

        def pair = ctx.findAfter(c, '"')
        if (pair && !ctx.near(pair, false, "Space") && !ctx.usages(pair, "Quoted")) {
          def newArgs = [c, ctx.coloredBetween(c, pair), pair]
          def descr = cons("Quoted").expect(["nameable":1, "_":0], cons("Appos")).consumes(0, 1, 2).demotes(0, 1, 2)
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