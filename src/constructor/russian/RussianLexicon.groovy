package constructor.russian

import org.codehaus.groovy.runtime.MetaClassHelper
import constructor.*

/**
 * @author peter
 */
class RussianLexicon extends Lexicon {
  static def subject = cons("Subject").consumes(1).identifyArgs([1:"nominative"]).semantics { args -> args[0]["agent"] = args[1]}
  static def object = cons("Obj").consumes(1).identifyArgs([1:"accusative"])
  static def appos = cons("Appos").form(["nameable":1, "Quoted":0]).semantics { it[1]["name"] = it[0]; it[1] }

  static Descriptor obj(String slot) {
    cons("Obj").consumes(1).
            form(["clause", "accusative"]).
            form(["accusative":1, "clause":0]).
            identifyArgs([1:"accusative"]).semantics {it[0][slot] = it[1]; it[0]}
  }

  static Descriptor subj(String agr) {
    cons("Subject").
            consumes(1).
            form([["nominative", agr]:1, "clause":0]).
            form(["clause", ["nominative", agr]]).
            identifyArgs([1:"nominative"]).
            semantics { args -> args[0]["agent"] = args[1]}
  }

  def RussianLexicon() {
    specials()

    def timePeriodSpecifier = cons("When").form(["Prepos", "timed"]).form(["timed": 1, "Prepos": 0]).semantics {
      def f = new Frame("deadline")
      it[1]["time"] = f
      f["unit"] = it[0];
      it[1]
    }

    preposition("в течение", "genitive") { it.evokes(timePeriodSpecifier, 0) }

    def locationSpecifier = cons("Where").consumes(1).
            form(["locatable": 1, "Prepos": 0]).
            form(["Prepos", "locatable"]).
            semantics {it[1]["location"] = it[0]; it[0]}

    preposition("в", "prepositional") { it.evokes(locationSpecifier, 0) }

    def timeMomentSpecifier = cons("When").form(["timed": 1, "Prepos": 0]).semantics { it[1]["time"] = it[0]; it[1] }

    preposition("во", ["accusative", "time"]) { it.evokes(timeMomentSpecifier, 0)}
    preposition("за", "accusative") {it.evokes(timePeriodSpecifier, 0)}
    preposition("с", "genitive") {it.expect(["_", "clause"], cons("When"))}

    word("по").famous().variants(
            expect(["_", "его", "словам"],
                                        cons("По словам").consumes(0, 1, 2).
                                        expect(["_", ","], cons("Comma").consumes(1)).
                                        expect(["_", "clause"], cons("Source"))),
            evokes(prepos("dative").famous(), 0))

    nounStem('власт', 17) { it.nounObj("governed").aka("pl").frame("authorities") }
    nounStem('управ', 50) { it.aka("nameable").frame("council") }

    ending('и')
    ending('ы')

    noun("месяца", "genitive").frame("month")
    noun("вопрос", "accusative")
    noun("сносе", "prepositional").nounObj("undergoer").aka("locatable").frame("demolition")
    noun("сносу", "dative")
    noun("строений", "genitive").aka("locatable").semantics { def f = new Frame("building"); f["quantity"] = "plural"; f }
    noun("поселке", "prepositional").aka("nameable").frame("housing_development")
    noun("постройки", "accusative").aka("pl").semantics { def f = new Frame("building"); f["quantity"] = "plural"; f }
    noun("постройки", "genitive").aka("sg")
    noun("месяц", "accusative").frame("month")
    noun("журналистам", "dative").aka("animate").semantics { def f = new Frame("journalist"); f["quantity"] = "plural"; f } //todo hack
    noun("вторник", "accusative").aka("time").frame("Tuesday")
    noun("глава", "nominative").nounObj("governed").aka("masc").frame("head")
    noun("утра", "genitive")
    noun("строения", "genitive")
    noun("дом", "nominative").aka("3sg")
    noun("дома", "accusative").aka("pl")
    noun("дома", "genitive").aka("sg")
    noun("приставы", "nominative").aka("3pl")
    noun("жители", "nominative").aka("pl")
    noun("чиновник", "nominative").aka("masc")
    noun("добро", "accusative")
    noun("словам", "dative")
    noun("решения", "nominative").expect(["по": 1, "dative": 2, "_": 0], cons("About").consumes(1, 2))

    noun("мать", "nominative").aka("3sg")
    noun("мать", "accusative").aka("3sg")
    noun("дочь", "nominative").aka("3sg")
    noun("дочь", "accusative").aka("3sg")

    noun("мы", "nominative").aka("1pl").frame("we")
    noun("он", "nominative").aka("masc")
    noun("нам", "dative")

    adj("московской", "genitive") { it["part_of"] = "Москва" }
    adj("незаконных", "genitive") { it["legal"] = "-" }
    adj("незаконные", "accusative") { it["legal"] = "-" }
    adj("местные", "nominative")
    adj("судебные", "nominative")
    adj("родная", "nominative").aka("nom")
    adj("родную", "accusative").aka("acc")

    verb('любит', '3sg').evokes(obj("theme"), 0)

    def goalXComp = cons("XComp").consumes(1).form(["_", "infinitive"]).semantics { it[0]["goal"] = it[1]; it[1]["agent"] = it[0].ref("agent"); it[0] }
    verb('намерены', "pl").evokes(goalXComp, 0).frame("intention")
    verb("планируем", "1pl").evokes(goalXComp, 0).frame("intention")

    verb("планируется", "3sg").aka("locatable").expect(["_", "к", "dative"], cons("Oblique").consumes(1, 2))

    def directSpeech = cons("DirectSpeech").consumes(1, 2, 3).form(["Quoted": 1, ",": 2, "-": 3, "_": 0]).semantics { it[0]["message"] = it[1]; it[0] }
    verb("сообщил", "masc").aka("locatable", "timed").frame("tell").
            evokes(directSpeech, 0).
            expect(["_", ["dative", "animate"]], cons("Goal").consumes(1).semantics { it[0]["addressee"] = it[1]; it[0] }).
            expect(["_", ",", "что", "clause"], cons("Comp").consumes(1, 2, 3).demotes(1, 2))
    verb("сказал", "masc").evokes(directSpeech, 0)

    verb("демонтированы", "pl")
    verb("готовится", "3sg").expect(["к": 1, "dative": 2, "_": 0], cons("Oblique").consumes(1, 2))
    verb('дают', "3pl").
            expect(["_", "добро"], verb(cons("дать добро"), "3pl").consumes(1).replaces(0), true).
            expect(["dative": 1, "_": 0], cons("Goal").consumes(1)).
            evokes(obj("given"), 0, true)
    verb("сносим", "1pl").evokes(obj("undergoer"), 0)
    verb("снесли", "1pl").expect(["не", "_", "ни", "одного", "genitive"], cons("NegObj").consumes(0, 2, 3, 4))
    verb("проживали", "pl").aka("locatable")
    verb("есть", "pl").expect(["Prepos": 2, "_": 1, "nominative": 0], cons("Copula"))

    infinitive("решить").aka("timed").expect(["_", "вопрос", "о", "prepositional"], cons("Решить вопрос").replaces(0).consumes(0, 1, 2, 3).semantics { it[0]["problem"] = it[3]; it[0] }).frame("resolve_problem")
    infinitive("снести").aka("timed").evokes(obj("undergoer"), 0).frame("demolition")

    word('крылатское').famous().frame("Крылатское") //todo special handing for self-naming words
    word("речник").famous().frame("Речник")
    word("одного").famous()
    word("его").famous()
    word("не").famous()
    word("ни").famous()
    word("о").famous()
    word("все").expect(["_", "accusative"], cons("Quantifier").semantics { it[1]["scope"] = "all"; it[1]})
    word("виталий").famous().frame("Виталий").expect(["_", "Surname"],
                                                     cons("NameSurname").expect(["nominative": 1, "_": 0],
                                                                                cons("Named").semantics { it[1]["name"] = it[0]; it[1] }
                                                     ).semantics { def f = new Frame("HumanName"); f["first"] = it[0]; f["last"] = it[1]; f })
    word("никитин").frame("Никитин").famous().aka("Surname")
    word("что").famous()
    word("уже").expect(["_", "clause"], cons("Already").consumes(0))
    word("два").expect(["_", "genitive"], cons("Quantity").famous().consumes(0, 1).aka("nominative", "pl"))
    word("к").famous()
    word("третий").expect(["_", "nominative"], cons("Order").consumes(0))
    word("когда").expect(["_", "clause"], cons("XWhen").consumes(0).expect(["_", ",", "clause"], cons("When").consumes(1)))
    word("эти").expect(["_", "accusative"], cons("Demonstrative"))

    word("котором").aka("prepositional").evokes(relative("sg"), 0)
    word("которых").aka("genitive").evokes(relative("pl"), 0)

    word("бы").expect(["_", "clause"], cons("Subjunctive"))
    word("42").expect(["_", ["genitive", "sg"]], cons("Quantity").famous().consumes(0, 1).aka("nominative", "3sg", "pl", "NP"))
    word("37").expect(["_", ["genitive", "pl"]], cons("Quantity")).aka("number", "dative") //todo let numbers have any case, gender, number
    word("всего").expect(["_", "clause", "Quantity"], cons("Всего+Утверждение о количестве"))
    word("из").expect(["_": 1, "genitive": 2, "number": 0], cons("Subset"))

  }

  private static CompositeQuery expect(pattern, descr) {
    return new CompositeQuery().expect(pattern, descr)
  }

  private static CompositeQuery evokes(pattern, arg) {
    return new CompositeQuery().evokes(pattern, arg)
  }

  private Descriptor ending(String s) {
    return word(s).famous()
  }

  private def _addNoun(List<Noun> result, String _case, boolean plural) {
    def noun = new Noun("${MetaClassHelper.capitalize(_case)}Noun", _case).consumes(0, 1).demotes(0, 1).aka(plural ? "pl" : "sg")
    result << noun
    return noun
  }

  private void nounStem(String s, int cls, Closure c) {
    def stem = word(s)
    List<Noun> result = []
    if (cls == 17) {
      stem.expect(["_", "и"], _addNoun(result, "nominative", true))
    }
    if (cls == 50) {
      stem.expect(["_", "ы"], _addNoun(result, "genitive", false))
    }
    if (cls == 106) {
      stem.expect(["_", "йки"], _addNoun(result, "accusative", true), true)
      stem.expect(["_", "йки"], _addNoun(result, "genitive", false), true)
    }
    c(*result)
  }

  private Descriptor relative(String agr) {
    return cons("Relative").form([["NP", agr]: 1, ",": 2, "_": 0])
  }

  def _t(Descriptor d) { d.track() }

  private Descriptor infinitive(String s) {
    return word(s).famous().aka("infinitive", "clause")
  }

  private Descriptor verb(String s, String agr) {
    return verb(word(s), agr)
  }

  def verb(Descriptor descriptor, String agr) {
    return descriptor.aka("clause").evokes(subj(agr), 0)
  }

  private Descriptor preposition(String s, _case, Closure binding) {
    return preposition(word(s), _case, binding)
  }

  private Descriptor preposition(Descriptor base, _case, Closure binding) {
    return base.evokes(binding(prepos(_case)), 0)
  }

  private Descriptor prepos(_case) {
    return cons("Prepos").consumes(1).
            form([[], _case]).
            semantics {it[1]}
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
          def descr = cons("Quoted").evokes(appos, 0).consumes(0, 1, 2).demotes(0, 1, 2).semantics { it[1] }
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