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
        return ufiniteVerb(var, 'THINK', 'PRESENT') + uarg(var, poDat, poDatTopic) +
               unomArg(var, [agrNumber:'pl']) + uaccArg(var).xor(noArg(head:var))
      case "сидят":
        return ufiniteVerb(var, 'SIT', 'PRESENT') + unomArg(var, [agrNumber:'pl'])
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
        return uv(var, type:'MOVE') +
               u(adverb(head:var), adverbialPhrase(content:var, head:v[verbHolder]), verbHolder(head:v[verbHolder].lightVar)) +
               uarg(var, instr, instrArg2)
      case "забыл":
      case "забыла":
      case "забыли":
        def gender = word == 'забыли' ? 'pl' : word == 'забыла' ? 'fem' : 'masc'
        Map<String,String> agr = [agrGender:gender]
        return ufiniteVerb(var, 'FORGET', 'PAST') + unomArg(var, agr, v[nom]) +
               uv(v[nom], gender:gender, person:'3') +
               u(elaboration(elaboration:var), advObj(head:var, xor:t.a),
                       acc(noun:v[acc].lightVar, head:var, xor:t.a), accArg2(head:var, noun:v[acc]),
                       declOrQuestionComp(head:var, xor:t.a))
      case 'сломал':
      case 'сломала':
        return ufiniteVerb(var, 'BREAK', 'PAST') +
               unomArg(var, [agrNumber:'sg', agrGender:word == 'сломала' ? 'fem' : 'masc'], v[nom]) +
               uaccArg(var, v[acc]) + uarg(var, v[dat], dat) +
               u(dativePart(head:var, dat:v[dat])) +
               uv(v[nom], gender:(word == 'сломала' ? 'fem' : 'masc'))
      case "случился":
        return ufiniteVerb(var, 'HAPPEN', 'PAST',
                (sInstr):[noun:v[sInstr].lightVar, head:var], (sInstrExperiencer):[head:var, noun:v[sInstr]],
                (nom):[noun:v[nom].lightVar], (nomSubject):[head:var, noun:v[nom]])

      case "помнят":
      case "помнит":
        Map<String,String> agr = [agrGender:null]
        return ufiniteVerb(var, 'REMEMBER', 'PRESENT') +
               unomArg(var, agr, v[nom]) +
               uv(v[nom], rusNumber:(word == "помнят" ? 'pl' : 'sg'), person:'3') +
               uaccArg(var)
      case "может":
      case "могут":
        return ufiniteVerb(var, 'CAN', 'PRESENT', (control):[subj:v[nom], head:var]) + unomArg(var)
      case 'стали':
      case 'начали':
        return ufiniteVerb(var, 'BEGIN', 'PAST', (control):[head:var]) + unomArg(var)
      case 'свалился':
        return ufiniteVerb(var, 'FALL', 'PAST') + unomArg(var, [agrNumber:'sg', agrGender:'masc']) + uarg(var, sGen, sGenSource)
      case "отправился":
        return ufiniteVerb(var, 'GO_OFF', 'PAST')  + unomArg(var) + uarg(var, kDat, kDatGoal)
      case "пошли":
        return ufiniteVerb(var, 'GO', 'PAST') + unomArg(var) + uarg(var, vAcc, vAccGoal)
      case "спорили":
        return ufiniteVerb(var, 'ARGUE', 'PAST') + unomArg(var, [agrNumber:'pl'])
      case "обнаружил":
      case "обнаружила":
      case "обнаружили":
        return ufiniteVerb(var, 'DISCOVER', 'PAST', (declOrQuestionComp):[head:var]) + unomArg(var)
      case "улыбнулась":
        return ufiniteVerb(var, 'SMILE', 'PAST') + unomArg(var)
      case "сказал":
      case "сказала":
        return ufiniteVerb(var, 'SAY', 'PAST') + unomArg(var) + uarg(var, dat, datAddressee) +
               u(declOrQuestionComp(head:var)).xor(directSpeech(head:var))
      case "вынул":
      case "вынула":
        return ufiniteVerb(var, 'TAKE_OUT', 'PAST') + unomArg(var) + uaccArg(var) + uarg(var, izGen, izGenSource)
      case "показались":
        return ufiniteVerb(var, 'SEEM', 'PAST',
                (dat):[noun:v[dat].lightVar, head:var], (participleArg):[head:var]) + unomArg(var)
      case "идет":
      case "идёт":
        Construction goes = varCxt(type:'GO')
        return uv(var, time:'PRESENT') + u(comeScalarly(verb:var, xor:t.ab), goes(var:var, xor:t.ad)) +
               uarg(var, vPrep, vPrepCondition).xor(noArg(head:var)) +
               unomArg(var, [:], v[nom]) + uv(v[nom], person:'3') +
               u(poDat(head:var)).xor(noArg(head:var)) +
               u(vAcc(noun:v[vAcc].lightVar, xor:t.b, head:var), vAccGoal(head:var, noun:v[vAcc])).xor(noArg(head:var)) +
               u(verbHolder(head:var), verbalModifier(head:var), sentenceHolder(head:var),
                       posleGen(head:var, xor:t.d), ransheGen(head:var, xor:t.d),
                       conditionComp(head:var), absTime(head:var)) +
               u(complementizer(content:var)).xor(question(content:var))
      case "следовало":
        return uv(var, time:'PAST') + unomArg(var) +
               u(comeScalarly(verb:var), verbHolder(head:var), verbalModifier(head:var), sentenceHolder(head:var),
                       posleGen(head:var, xor:t.d), relTime(head:var), elaboration(elaboration:var))
      case 'был':
        def subj = new Variable().lightVar
        return uv(var, time:'PAST') +
               u(naPrep(head:subj, copula:var), sentenceHolder(head:var), uGen(head:subj, copula:var), nom(noun:subj, head:var))
      case 'есть':
        def subj = new Variable().lightVar
        return uv(var, time:'PRESENT') +
               u(naPrep(head:subj, copula:var), sentenceHolder(head:var), uGen(head:subj, copula:var), nom(noun:subj, head:var))
    }
    return null
  }

  static Update ufiniteVerb(Variable verb, String type, String tense) {
    ufiniteVerb([:], verb, type, tense)
  }

  static Update ufiniteVerb(Map<Construction, Map> args, Variable verb, String type, String tense) {
    return u() + args + uv(verb, type:type, time:tense) +
           u(subjunctive(head:verb), verbHolder(head:verb), sentenceHolder(head:verb), verbalModifier(head:verb)) +
           u(relTime(head:verb), also(head:verb), adverb(head:verb)) +
           u(complementizer(content:verb)).xor(question(content:verb))
  }

  static Update unomArg(Variable head, Map<String,String> agr = [:], Variable arg = new Variable()) {
    u(nom([noun:arg.lightVar, head:head] + agr), nomSubject(head:head, noun:arg), reflexiveHolder(noun:arg))
  }

  static Update uaccArg(Variable head, Variable arg = new Variable()) { uarg(head, arg, acc, accArg2) }

}
