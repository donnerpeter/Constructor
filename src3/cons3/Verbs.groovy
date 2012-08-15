package cons3

import static cons3.Parser.*
import static cons3.RussianConstructions.*

/**
 * @author peter
 */
class Verbs {
  static Update parseUpdate(String word) {
    Variable var = new Variable()
    Vars v = new Vars()
    Tokens t = new Tokens()
    switch (word) {
      case "остановились":
        return ufiniteVerb(var, 'STOP', 'PAST') + unomArg(var)
      case "думают":
        return ufiniteVerb(var, 'THINK', 'PRESENT') + uarg(var, poDat, poDatTopic) + unomArg(var) + uaccArg(var)
      case "сидят":
        return ufiniteVerb(var, 'SIT', 'PRESENT') + unomArg(var)
      case "спросил":
      case "спросили":
        return ufiniteVerb(var, 'ASK', 'PAST') + uaccArg(var) + unomArg(var) + u(declOrQuestionComp(head:var)) + uarg(var, oPrep, oPrepTopic)
      case "делал":
      case "делали":
        return ufiniteVerb(var, 'DO', 'PAST') + uaccArg(var) + unomArg(var)
      case "поблагодарили":
        return ufiniteVerb(var, 'THANK', 'PAST') + uaccArg(var) + unomArg(var)
      case "разошлись":
        return ufiniteVerb(var, 'DISPERSE', 'PAST') + unomArg(var) + uarg(var, poDat, poDatGoal)
      case "выбежали":
        return ufiniteVerb(var, 'RUN_OUT', 'PAST') + uarg(var, izGen, izGenSource) + unomArg(var) + uarg(var, sInstr, sInstrMood)
      case "приуныли":
        return ufiniteVerb(var, 'GET_SAD', 'PAST') + unomArg(var) + u(reasonComp(head:var))
      case "отвлекло":
        return ufiniteVerb(var, 'DISTRACT', 'PAST') + uarg(var, otGen, otGenTheme) + unomArg(var) + uaccArg(var)
      case "подвигала":
        return ufiniteVerb(var, 'MOVE', 'PAST') + unomArg(var) + uarg(var, instr, instrArg2)
      case "подвигав":
        return uv(var, type:'MOVE') + u(adverb(head:var), adverbialPhrase(content:var)) + uarg(var, instr, instrArg2)
      case "забыл":
      case "забыла":
      case "забыли":
        return ufiniteVerb(var, 'FORGET', 'PAST') + unomArg(var) +
               u(elaboration(elaboration:var), advObj(head:var, xor:t.a),
                       acc(noun:v[acc].lightVar, head:var, xor:t.a), accArg2(head:var, noun:v[acc]),
                       declOrQuestionComp(head:var, xor:t.a))
      case 'сломал':
      case 'сломала':
        return ufiniteVerb(var, 'BREAK', 'PAST') +
               unomArg(var, [agrNumber:'sg'], v[nom]) +
               uaccArg(var, v[acc]) + uarg(var, v[dat], dat) +
               u(dativePart(head:var, acc:v[acc], dat:v[dat])) +
               uv(v[nom], gender:(word == 'сломала' ? 'fem' : 'masc'))
    }
    return null
  }

  static Update ufiniteVerb(Variable verb, String type, String tense) {
    ufiniteVerb([:], verb, type, tense)
  }

  static Update ufiniteVerb(Map<Construction, Map> args, Variable verb, String type, String tense) {
    return u() + args + uv(verb, type:type, time:tense) +
           u(subjunctive(head:verb), verbHolder(head:verb), sentenceHolder(head:verb)) +
           u(relTime(head:verb), also(head:verb), adverb(head:verb)) +
           u(complementizer(content:verb)).xor(question(content:verb))
  }

  static Update unomArg(Variable head, Map<String,String> agr = [:], Variable arg = new Variable()) {
    u(nom([noun:arg.lightVar, head:head] + agr), nomSubject(head:head, noun:arg), reflexiveHolder(noun:arg))
  }

  static Update uaccArg(Variable head, Variable arg = new Variable()) { uarg(head, arg, acc, accArg2) }

}
