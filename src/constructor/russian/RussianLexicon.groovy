package constructor.russian

import constructor.*

/**
 * @author peter
 */
class RussianLexicon extends Lexicon {
  static def subject = cons("Subject").consumes(1).identifyArgs([1:"nominative"]).semantics { args -> args[0]["agent"] = args[1]}
  static def object = cons("Obj").consumes(1).identifyArgs([1:"accusative"])

  static Descriptor obj(String slot) {
    cons("Obj").consumes(1).identifyArgs([1:"accusative"]).semantics {it[0][slot] = it[1]; it[0]}
  }

  def RussianLexicon() {
    specials()
    preposition("в течение", "genitive") { it.expect(["_", "clause"], cons("When").semantics {
      def f = new Frame("deadline")
      it[1]["time"] = f
      f["unit"] = it[0];
      it[1]
    }) }
   // preposition("о", "prepositional") {it.expect(["noun": 1, "_": 0], cons("About"))}
    preposition("в", "prepositional") { it.
                    expect([["noun", "locatable"]: 1, "_": 0], cons("Where").consumes(1).semantics {it[1]["location"] = it[0]; it[0]}).
                    expect([["clause", "locatable"]: 1, "_": 0], cons("Where").consumes(1).semantics {it[1]["location"] = it[0]; it[0]}).
                    expect(["_", ["clause", "locatable"]], cons("Where").consumes(1))
                    }
    preposition("во", ["accusative", "time"]) { it.expect(["clause": 1, "_": 0], cons("When").semantics { it[1]["time"] = it[0]; it[1] } )}
    preposition("за", "accusative") {it.expect(["clause": 1, "_": 0], cons("When").semantics{
      def f = new Frame("deadline")
      it[1]["time"] = f
      f["unit"] = it[0];
      it[1]
    })}
    preposition("с", "genitive") {it.expect(["_", "clause"], cons("When"))}
    preposition("по", "dative") { it.famous() }.famous()

    noun("Власти", "nominative").nounObj("governed").aka("pl").frame("authorities")
    noun("управы", "genitive").aka("nameable").frame("council")
    noun("месяца", "genitive").frame("month")
    noun("вопрос", "accusative")
    noun("сносе", "prepositional").nounObj("undergoer").aka("locatable").frame("demolition")
    noun("сносу", "dative")
    noun("строений", "genitive").aka("locatable").semantics { def f = new Frame("building"); f["quantity"] = "plural"; f }
    noun("поселке", "prepositional").aka("nameable").frame("housing_development")
    noun("постройки", "accusative").semantics { def f = new Frame("building"); f["quantity"] = "plural"; f }
    noun("постройки", "genitive")
    noun("месяц", "accusative").frame("month")
    noun("журналистам", "dative").aka("animate").semantics { def f = new Frame("journalist"); f["quantity"] = "plural"; f } //todo hack
    noun("вторник", "accusative").aka("time").frame("Tuesday")
    noun("глава", "nominative").nounObj("governed").aka("masc").frame("head")
    noun("утра", "genitive")
    noun("строения", "genitive")
    noun("дом", "nominative").aka("3sg")
    noun("дома", "accusative")
    noun("дома", "genitive")
    noun("приставы", "nominative").aka("3pl")
    noun("жители", "nominative").aka("pl")
    noun("чиновник", "nominative").aka("masc")
    noun("добро", "accusative")
    noun("словам", "dative")
    noun("решения", "nominative").expect(["по":1, "dative":2, "_":0], cons("About").consumes(1, 2))

    noun("мать", "nominative").aka("3sg")
    noun("мать", "accusative").aka("3sg")
    noun("дочь", "nominative").aka("3sg")
    noun("дочь", "accusative").aka("3sg")

    noun("Мы", "nominative").aka("1pl").frame("we")
    noun("мы", "nominative").aka("1pl")
    noun("Он", "nominative").aka("masc")
    noun("нам", "dative")

    adj("московской", "genitive") { it["part_of"] = "Москва" }
    adj("незаконных", "genitive") { it["legal"] = "-" }
    adj("незаконные", "accusative") { it["legal"] = "-" }
    adj("местные", "nominative")
    adj("судебные", "nominative")
    adj("родная", "nominative").aka("nom")
    adj("родную", "accusative").aka("acc")

    store(new TransitiveVerb('любит', '3sg'))
    
    verb('намерены', "pl").
            expect(["_", "infinitive"], cons("XComp").consumes(1).semantics { it[0]["goal"] = it[1]; it[1]["agent"] = it[0].ref("agent"); it[0] }).
            frame("intention")

    verb("планируем", "1pl").
            expect(["_", "infinitive"], cons("XComp").consumes(1).semantics { it[0]["goal"] = it[1]; it[1]["agent"] = it[0].ref("agent"); it[0] }).
            frame("intention")
    verb("планируется", "3sg").aka("locatable").expect(["_", "к", "dative"], cons("Oblique").consumes(1, 2))

    verb("сообщил", "masc").aka("locatable").frame("tell").
            expect(["Quoted": 1, ",": 2, "-": 3, "_": 0], cons("DirectSpeech").consumes(1, 2, 3).semantics { it[0]["message"] = it[1]; it[0] } ).
            expect(["_", ["dative", "animate"]], cons("Goal").consumes(1).semantics { it[0]["addressee"] = it[1]; it[0] }).
            expect(["_", ",", "что", "clause"], cons("Comp").consumes(1, 2, 3).demotes(1, 2))
    verb("сказал", "masc").expect(["Quoted": 1, ",": 2, "-": 3, "_": 0], cons("DirectSpeech").consumes(1, 2, 3))
    verb("демонтированы", "pl")
    verb("готовится", "3sg").expect(["к":1, "dative":2, "_":0], cons("Oblique").consumes(1, 2))
    verb('дают', "3pl").
            expect(["_", "добро"], cons("дать добро").consumes(1).suppresses(0, object.name)).
            expect(["dative":1, "_":0], cons("Goal").consumes(1)).
            expect(["_", "accusative"], object)
    verb("сносим", "1pl").expect(["accusative":1, "_":0], object)
    verb("снесли", "1pl").expect(["не", "_", "ни", "одного", "genitive"], cons("NegObj").consumes(0, 2, 3, 4))
    verb("проживали", "pl").aka("locatable")
    verb("есть", "pl").expect(["Prepos":2, "_":1, "nominative":0], cons("Copula"))

    infinitive("решить").expect(["_", "вопрос", "о", "prepositional"], cons("Решить вопрос").consumes(0, 1, 2, 3).semantics { it[0]["problem"] = it[3]; it[0] }).frame("resolve_problem")
    infinitive("снести").expect(["accusative": 1, "_": 0], obj("undergoer")).frame("demolition")

    word('Крылатское').famous().frame("Крылатское") //todo special handing for self-naming words
    word("Речник").famous().frame("Речник")
    word("одного").famous()
    word("его").famous()
    word("не").famous()
    word("ни").famous()
    word("о").famous()
    word("все").expect(["_", "accusative"], cons("Quantifier").semantics { it[1]["scope"] = "all"; it[1]})
    word("Виталий").famous().frame("Виталий").expect(["_", "Surname"],
            cons("NameSurname").expect(["nominative": 1, "_": 0],
                    cons("Named").semantics { it[1]["name"] = it[0]; it[1] }
            ).semantics { def f = new Frame("HumanName"); f["first"] = it[0]; f["last"] = it[1]; f })
    word("Никитин").frame("Никитин").famous().aka("Surname")
    word("что").famous()
    word("уже").expect(["_", "clause"], cons("Already").consumes(0))
    word("два").expect(["_", "genitive"], cons("Quantity").famous().consumes(0, 1).aka("nominative", "pl"))
    word("к").famous()
    word("третий").expect(["_", "nominative"], cons("Order").consumes(0))
    word("Когда").expect(["_", "clause"], cons("XWhen").consumes(0).expect(["_", ",", "clause"], cons("When").consumes(1)))
    word("эти").expect(["_", "accusative"], cons("Demonstrative"))
    word("котором").aka("prepositional").expect(["noun":1, ",":2, "_":0], cons("Relative"))
    word("которых").aka("genitive").expect(["NP":1, ",":2, "_":0], cons("Relative"))
    word("бы").expect(["_", "clause"], cons("Subjunctive"))
    word("По").expect(["_", "его", "словам", ","], cons("По словам").consumes(0, 1, 2, 3).expect(["_", "clause"], cons("Source")))
    word("42").expect(["_", "genitive"], cons("Quantity").famous().consumes(0, 1).aka("nominative", "3sg", "NP"))
    word("37").expect(["_", "genitive"], cons("Quantity")).aka("number", "dative") //todo let numbers have any case
    word("всего").expect(["_", "clause", "Quantity"], cons("Всего+Утверждение о количестве"))
    word("из").expect(["_":1, "genitive":2, "number":0], cons("Subset"))

  }

  def _t(Descriptor d) { d.track() }

  private Descriptor infinitive(String s) {
    return word(s).famous().aka("infinitive", "clause")
  }

  private Descriptor verb(String s, String agr) {
    return verb(word(s), agr)
  }

  def verb(Descriptor descriptor, String agr) {
    return descriptor.aka("clause").
            expect([["nominative", agr]: 1, "_": 0], subject).
            expect(["_", ["nominative", agr]], subject)
  }

  private Descriptor preposition(String s, _case, Closure binding) {
    return word(s).expect(["_", _case], binding(cons("Prepos").consumes(1).semantics {it[1]}))
  }

  private Descriptor adj(String name, String _case, Closure semantics = {}) {
    return word(name).expect(["_", _case], cons("AdjNoun").identifyArgs([1:_case]).semantics { semantics(it[1]); it[1] })
  }

  Noun noun(String name, String _case) {
    return (Noun) store(new Noun(name, _case))
  }

  Descriptor word(String name) {
    return store(cons(name))
  }

  private def store(Descriptor cb) {
    storage.get(cb.name, []) << cb
    return cb
  }

  static Descriptor cons(String name) {
    return new Descriptor(name)
  }

  def specials() {
    store(new Descriptor('"') {

      boolean activate(Construction c, ParsingContext ctx) {
        if (ctx.usages(c, "Quoted")) {
          return true
        }

        def pair = ctx.findAfter(c, '"')
        if (pair && !ctx.near(pair, false, "Space") && !ctx.usages(pair, "Quoted")) { // todo true handling of nested quotes
          def newArgs = [c, ctx.coloredBetween(c, pair), pair]
          def appos = cons("Appos").semantics { it[1]["name"] = it[0]; it[1] }
          def descr = cons("Quoted").expect(["nameable":1, "_":0], appos).consumes(0, 1, 2).demotes(0, 1, 2).semantics { it[1] }
          ctx.addConstruction(descr.build(newArgs))
          return true
        }
        return false
      }

    }.famous())

    word(',').famous()
    word('-').famous()

  }

}