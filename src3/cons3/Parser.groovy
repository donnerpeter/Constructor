package cons3
import static cons3.Construction.cxt
import static cons3.Construction.noArg
import static cons3.RussianConstructions.*
/**
 * @author peter
 */
class Parser {
  String log = ""

  void printLog() {
    println "\nLog:\n\n$log\n"
  }

  Chart parse(String text) {
    ParsingState state = ParsingState.EMPTY
    def tokenizer = new StringTokenizer(text, """ '":,.?!""", true)
    for (String w in tokenizer) {
      if (w != ' ') {
        def logged = state.appendLog("$w ${'-' * (100 - w.size())}\n")
        state = handleWord(w.toLowerCase().trim(), logged)
      }
    }
    log = state.logString
    return state.chart
  }

  static ParsingState handleWord(String word, ParsingState state) {
    Update _update = Verbs.parseUpdate(word) ?: parseUpdate(word)
    if (_update) {
      return _update.apply(state)
    }
    if (Util.parseNumber(word) != null) {
      return handleNumber(word, state)
    }

    Tokens t = new Tokens()
    Vars v = new Vars()
    Variable var = new Variable()

    switch (word) {
      case "летний":
        return state.apply((adjective):[nounFrame:var.lightVar, rel:'timeAnchor', val:'SUMMER', xor:t.a], (acc):[noun:var.lightVar], (summerGarden):[summer:true, xor:t.a])
      case "большой":
        return state.apply((adjective):[nounFrame:var.lightVar, rel:'size', val:'BIG'], (instr):[noun:var.lightVar], (acc):[noun:var.lightVar])
      case "нашем":
        state = state.assign(var, 'type', 'WE')
        return state.apply((possessive):[possessor:var])
      case "своим":
        return state.apply((reflexiveHolder):[noun:var.lightVar], (possessive):[possessor:var])
      case "гастроном": return state.apply((RussianConstructions.word):[word:word])
      case "сад":

        state = state.apply((typeCxt('GARDEN')):[var:var],
                (nom):[noun:var, xor:t.a], (acc):[noun:var, xor:t.a],
                (summerGarden):[garden:var], (naPrep):[head:var], (declOrQuestionComp):[head:var, rel:'relative'])
        return state //todo one noun frame - several cases
      case "магазина":
        return state.apply((typeCxt('SHOP')):[var:var], (gen):[noun:var],
                (naPrep):[head:var], (quotedName):[noun:var], (declOrQuestionComp):[head:var, rel:'relative'])
      case ":":
        Variable newVerb = new Variable()
        return state.apply(verbHolder(head:var.lightVar, hasColon:true), sentenceHolder(mustStart:true, head:newVerb.lightVar),
                directSpeech(xor:t.a, message:newVerb), elaboration(head:var, xor:t.a), colon([:])).advanceSection()
      case "мое": return state.apply((typeCxt('ME')):[var:var], (possessive):[possessor:var, xor:t.a])
      case "моему": return state.apply((typeCxt('ME')):[var:var], (possessive):[possessor:var])
      case "нашего": return state.apply((typeCxt('WE')):[var:var], (possessive):[possessor:var])
      case "а":
      case "но":
        return u(seq(conj:'but'), clauseEllipsis(but:true),
                verbEmphasis(emphasis:word == 'а' ? 'andEmphasis' : 'butEmphasis', verb:var),
                sentenceHolder(head:var.lightVar), verbHolder(head:var.lightVar)).apply(state)
      case "тут":
        return state.assign(var.lightVar, 'emphasis', 'true').apply(verbHolder(head:var.lightVar))
      case "все":
        return state.apply((nom):[noun:v[nom].lightVar], (varCxt(quantifier:'ALL')):[var:v[nom]])
      case "дальше":
        return state.apply((comeScalarly):[order:'AFTER', xor: t.ab], (typeCxt('NEXT')):[var:var, xor:t.b], (advObj):[adv:var, xor:t.a])
      case "их":
        return state.apply((typeCxt('THEY')):[var:var], (possessive):[possessor:var, xor:t.a], (acc):[noun:var, xor:t.a])
      case "его":
        return state.apply((typeCxt('HE')):[var:var], (possessive):[possessor:var, xor:t.a], (acc):[noun:var, xor:t.a])
      case "порядок":
        return state.apply((typeCxt('ORDER')):[var:var], (acc):[noun:var], (gen):[noun:v[gen].lightVar, head:var], (genCriterion):[head:var, noun:v[gen]])
      case "тоже":
        return state.apply((also):[hasAlso:true])
      case "не":
        return state.apply((negation):[:])
      case "дойдя":
        state = state.assign(var, 'type', 'COME_TO')
        return state.apply(
                (vPrep):[head:var, noun:v[vPrep].lightVar], (vPrepDomain):[head:var, noun:v[vPrep]],
                (doGen):[head:var, noun:v[doGen].lightVar], (doGenGoal):[head:var, noun:v[doGen]],
                (adverbialPhrase):[content:var, head:v[verbHolder]], (verbalModifier):[head:v[verbHolder].lightVar, hasModifier:true])
      case "вдумываясь":
        state = state.assign(var, 'type', 'THINK')
        return state.apply((vAcc):[noun:v[vAcc].lightVar, head:var],
                (vAccTheme):[head:var, noun:v[vAcc]],
                (adverbialPhrase):[content:var, head:v[verbHolder]], (verbalModifier):[head:v[verbHolder].lightVar, hasModifier:true])
      case "нужно":
        return state.assign(var, 'type', 'NEED').apply((acc):[noun:v[acc].lightVar, head:var], (accArg2):[head:var, noun:v[acc]],
                (datArg1):[head:var, noun:v[dat]], (dat):[noun:v[dat].lightVar],
                (adverbPred):[adverb:var],
                (verbHolder):[head:var], (question):[content:var])
      case ",":
        def verb = v[verbHolder].lightVar
        return state.apply(
                comma(hasComma:true), seq(conj:',', xor:t.ab),
                verbHolder(hasComma:true),
                verbalModifier(head:verb, xor:t.b),
                reasonComp(hasComma:true, xor:t.a),
                adverbialPhrase(comma1:true, xor:t.a), adverbialPhrase(comma2:true, xor:t.a),
                poDatLuck(comma1:true, xor:t.a, head:verb), poDatLuck(comma2:true, xor:t.a, head:verb),
                accordingTo(comma1:true, xor:t.a, head:verb), accordingTo(comma2:true, xor:t.a, head:verb),
                clauseEllipsis(prevHistory:state)
        ).advanceSection()
      case "если":
        return state.apply((conditionComp):[wh:'if', comp:v[verbHolder]],
                (verbHolder):[head:v[verbHolder].lightVar, newClause:true])
      case "когда":
        return state.apply((conditionComp):[wh:'when', comp:v[verbHolder]],
                (verbHolder):[head:v[verbHolder].lightVar, newClause:true])
      case "это":
        return state.apply((nom):[noun:var, xor:t.a], (acc):[noun:var, xor:t.a], (typeCxt('THAT')):[var:var])
      case "-":
        state = state.advanceSection()
        return state.apply((directSpeech):[hasDash:true],
                (questionVariants):[hasDash:true],
                (clauseEllipsis):[interceptor:new EllipsisInterceptor(state)])
        return state
      case ".":
        return state.apply((dot):[head:var], (sentenceHolder):[head:var.lightVar, mustFinish:true]).advanceSection()
      case 'каково':
        return state.assign(var, 'type', 'wh').apply((shortAdj):[copula: v['copula'], value:var, noun:v[nom]],
                (nom):[noun:v[nom].lightVar, head:var],
                (conditionComp):[head:v['copula']])
      case 'бы':
        return state.apply((subjunctive):[subjunctive:true])
      case 'было':
        return state.apply((shortAdj):[time:'PAST', copula:var.lightVar], (timedModality):[time:'PAST'], (adverbPred):[time:'PAST'])
      case 'так':
        return state.apply((reasonComp):[reason:var], (verbHolder):[head:var.lightVar, newClause:true])
      case 'обе':
        def num = new Variable()
        return state.assign(num, 'type', 'BOTH').apply((acc):[noun:var],
                (numQuantifier):[num:num, prev:state, noun:var.lightVar, innerCase:gen, outerNoun:var])
      case 'по-моему':
        def me = new Variable()
        return state.assign(var, 'type', 'OPINION').assign(var, 'arg1', me).assign(me, 'type', 'ME').
                apply(accordingTo(content:var, head:v[verbHolder]), verbalModifier(head:v[verbHolder].lightVar, hasModifier:true))
      case '?': return state.apply((question):[:])
      case '"':
        return state.apply((quote):[:])
      case 'лишенными':
        state = state.assign(var, 'type', 'LACK')
        return state.apply((participleArg):[participle:var],
                (gen):[noun:v[gen].lightVar, head:var], (genArg2):[head:var, noun:v[gen]])
      case 'бессмысленными':
        state = state.assign(var, 'type', 'MEANINGLESS')
        return state.apply((participleArg):[participle:var])
    }
    return state
  }

  //todo generic noun treatment for numbers
  private static ParsingState handleNumber(String word, ParsingState state) {
    Variable noun = new Variable()
    Variable num = new Variable()

    def sem = numberTypeCxt(noun, word)

    List<Construction> cases = [nom, acc, gen]

    def qv = state[questionVariants]
    if (qv?.questioned && !qv?.seq) {
      def seqVar = new Variable()
      state = state.apply((questionVariants):[seq:seqVar])
      Update update = u(sem(var:noun), seq(multi:seqVar, second:noun))
      for (caze in cases) {
        update = update.addCxt(caze, noun:seqVar)
      }
      return update.apply(state)
    }

    Tokens t = new Tokens()
    return state.apply(cases.collectEntries { [it, [noun:noun, xor:t.a]] } +
                       [(sem):[var:noun, xor:t.bd],
                               (numQuantifier):[num:num, xor:t.b, noun:noun.lightVar, outerNoun:noun, prev:state, innerCase:(word == '1' ? acc : gen)],
                               (numberTypeCxt(num, word)):[xor:t.d, var:num]
                       ])
  }

  static Update parseUpdate(String word) {
    Tokens t = new Tokens()
    Variable var = new Variable()
    Vars v = new Vars()
    switch (word) {
      case "удивительный": return adj(nom, 'property', 'AMAZING')
      case "коммерческий": return adj(acc, 'kind', 'COMMERCIAL')
      case "маленький": return adj(acc, 'size', 'LITTLE')
      case "какой-то": return adj(nom, 'determiner', 'SOME')
      case "большим": return adj(instr, 'size', 'BIG')
      case "этому": return adj(dat, 'determiner', 'THIS')
      case "всякого": return adj(gen, 'determiner', 'ANY')
      case "скромному": return adj(dat, 'quality', 'HUMBLE')
      case "том": return adj(prep, 'determiner', 'THAT')
      case "васи":
        return unoun(gen, var, null) + uv(var, name:'Вася')
      case "знаменской": // todo a unified treatment for street names
      case "бассейной":
        return commonCase(gen, var.lightVar) + uv(var, type:'STREET') + u(adjective(nounFrame:var, rel:'name', val:word[0..-3]+"ая"))
      case 'себе':
        return upronoun(dat, var, null) + u(reflexiveHolder(noun:var.lightVar))
      case 'мне':
        return upronoun(dat, var, 'ME')
      case "мы": return upronoun(nom, var, 'WE')
      case "нам": return upronoun(dat, var, 'WE')
      case "я": return upronoun(nom, var, 'ME')
      case "нас": return upronoun(acc, var, 'WE')
      case "он": return upronoun(nom, var, 'HE')
      case "она": return upronoun(nom, var, 'SHE')
      case "они": return upronoun(nom, var, 'THEY')
      case "семь": return numeral(t, '7')
      case "восемь": return numeral(t, '8')
      case "два": return numeral(t, '2')
      case "три": return numeral(t, '3')
      case "один": return numeral(t, '1')
      case '6-ти':
      case 'шести':
        return unoun(gen, var, '6')
      case '5-ти':
        return unoun(gen, var, '5')
      case "магазин":
        return uv(var, type:'SHOP') + u(nom(noun:var)).xor(acc(noun:var)) + u(naPrep(head:var), quotedName(noun:var), declOrQuestionComp(head:var, rel:'relative'))
      case "счета": return unoun(gen, var, 'COUNTING')
      case "случай": return unoun(nom, var, 'THING') //todo случай=CASE or THING
      case "поводу": return unoun(dat, var, 'MATTER')
      case "рта": return unoun(gen, var, 'MOUTH')
      case "молоточек": return unoun(acc, var, 'HAMMER')
      case "радостью": return unoun( instr, var, 'JOY')
      case "облегчением": return unoun(instr, var, 'RELIEF')
    //case "улицы": return unoun(gen, var, 'STREET') todo улицы in conj
      case "скамейки": return unoun(gen, var, 'BENCH')
      case "ребенок": return unoun(nom, var, 'CHILD')
      case "сада": return unoun(gen, var, 'GARDEN')
      case "мной": return unoun(instr, var, 'ME')
      case "меня": return unoun(gen, var, 'ME')
      case "соседей": return unoun(acc, var, 'NEIGHBOURS')
      case "соседям": return unoun(dat, var, 'NEIGHBOURS')
      case "арбуз": return unoun(nom, var, 'WATER_MELON')
      case "кассиршу": return unoun(acc, var, 'CASHIER')
      case "деревья": return unoun(acc, var, 'TREES')
      case "деньги": return unoun(acc, var, 'MONEY')
      case "ее":
      case "её":
        return uv(type:'SHE', var) + u(possessive(possessor:var), acc(noun:var))
      case "семи": return unoun(gen, var, '7')
      case "восьми": return unoun(gen, var, '8')
      case "счете": return unoun(prep, var, 'COUNTING')
      case "работы": return unoun(gen, var, 'WORK')
      case "носом": return unoun(instr, var, 'NOSE')
      case "пальцев": return unoun(gen, var, 'FINGERS')
      case "пальца": return unoun(gen, var, 'FINGERS')
      case "палец": return unoun(acc, var, 'FINGER')
      case "челюсти":
        return u(gen(noun:var, xor:t.ab), acc(noun:var, xor:t.a)) + uv(type:'JAWS', var)
      case "челюстью": return unoun(instr, var, 'JAW')
      case "челюсть":
        return u(acc(noun:var)) + uv(type:'JAW', var)
      case "вдруг": return uadv('manner', 'SUDDENLY')
      case "уже": return uadv('anchor', 'ALREADY')
      case "опять": return uadv('anchor', 'AGAIN')
      case "слегка": return uadv('manner', 'SLIGHTLY')
      case "долго": return uadv('duration', 'LONG')
      case "грустно": return uadv('manner', 'SADLY')
      case "просто": return uadv('manner', 'JUST')
      case "случае": return unoun(prep, var, 'CASE', (conditionComp):[head:var])
      case "удивление": return unoun(nom, var, 'AMAZE') + uposs(var, possArg1)
      case "недоумении": return unoun(prep, var, 'PREDICAMENT') + uposs(var, possArg1)
      case "спора": return unoun(gen, var, 'ARGUE') + uposs(var, possArg1)
      case "смысла": return unoun(gen, var, 'MEANING')
      case "углу":
        return unoun(prep, var, 'CORNER') + uarg(var, gen, genArg1)
      case "кассир": return unoun(nom, var, null) + uv(var, type:'CASHIER', gender:'masc')
      case "кассирша": return unoun(nom, var, null) + uv(var, type:'CASHIER', gender:'fem')
      case "кассирши": return unoun(gen, var, 'CASHIER') + uarg(var, gen, genArg1).optional()
      case "одних": return unoun(gen, var, 'SOME')
      case "других":
        return unoun(gen, var, 'OTHERS')
      case "мнению":
        return unoun(dat, var, 'OPINION') +
               u(gen(head:var, noun:v[gen].lightVar, xor:t.b), possessive(head:var, possessor:v[gen].lightVar, xor:t.b), noArg(reason:'poss', xor:t.b)) +
               u(genArg1(head:var, noun:v[gen])) +
               u(boxedForPreposition(prep:'по', boxed:[accordingTo(content:var, xor:t.a)]))
      case "словам":
        return unoun(dat, var, 'WORDS') +
               u(gen(head:var, noun:v[gen].lightVar, xor:t.b), possessive(head:var, possessor:v[gen].lightVar, xor:t.b), noArg(reason:'poss', xor:t.b)) +
               u(genAuthor(head:var, noun:v[gen])) +
               u(boxedForPreposition(prep:'по', boxed:[accordingTo(content:var, xor:t.a)]))
      case "слова":
        return uv(var, type:'WORDS') + unoun(acc, var, null).xor(unoun(nom, var, null)) +
               uarg(var, v[gen], gen, genAuthor) + uposs(var, v[gen], possAuthor)
      case "счастию":
        return unoun(dat, var, 'LUCK') + u(boxedForPreposition(prep:'по', boxed:[poDatLuck(content:var)]))
      case "потом":
        return u(comeScalarly(order:'AFTER')).xor(relTime(relTime:'AFTER'))
      case "домам": return unoun(dat, var, 'HOMES') + uposs(var, possOwner)
      case "комнатам": return unoun(dat, var, 'ROOMS') + uposs(var, possOwner)
      case "квартирам": return unoun(dat, var, 'APARTMENTS') + uposs(var, possOwner)
      case "офисам": return unoun(dat, var, 'OFFICES') + uposs(var, possOwner)
      case "после":
        Variable noun = new Variable()
        return u(comeScalarly(order:'AFTER'), posleGen(noun:noun)).xor(absTime(rel:'AFTER', noun:noun)) + u(gen(noun:noun.lightVar, head:var))
      case "раньше":
        Variable noun = new Variable()
        return u(comeScalarly(order:'EARLIER'), ransheGen(noun:noun)).xor(relTime(relTime:'BEFORE', xor:t.a)) + u(gen(noun:noun.lightVar, head:var, xor:t.a))
      case "что":
        return u(declOrQuestionComp(comp:v[v])) +
               (whWord(var, v[v], false) + u(acc(noun:var)).xor(nom(noun:var, agrNumber:'sg', agrGender:'neu'))).xor(complementizer(frame:v[v]))
      case "кому":
        return u(declOrQuestionComp(comp:v[v])) + whWord(var, v[v], true) + u(dat(noun:var))
      case "кого":
        return u(declOrQuestionComp(comp:v[v])) + whWord(var, v[v], true) + u(acc(noun:var))
      case "чем":
      case "чём":
        return u(declOrQuestionComp(comp:v[v])) + whWord(var, v[v], false) + u(prep(noun:var))
      case "вспомнить":
        return infinitive(var, 'RECALL', (acc):[noun:v[acc].lightVar, head:var], (accArg2):[head:var, noun:v[acc]])
      case "делать":
        return infinitive(var, 'DO', (acc):[noun:v[acc].lightVar, head:var], (accArg2):[head:var, noun:v[acc]])
      case "спорить":
        return infinitive([:], var, 'ARGUE')
      case "считать":
        return infinitive(var, 'COUNT', (acc):[noun:v[acc].lightVar, head:var], (accArg2):[head:var, noun:v[acc]])
      case "поливать":
        return infinitive(var, 'TO_WATER', (acc):[noun:v[acc].lightVar, head:var], (accArg2):[head:var, noun:v[acc]])
      case "танцевать":
        return infinitive([:], var, 'DANCE')
      case "спросить":
        return infinitive(var, 'ASK', (acc):[noun:v[acc].lightVar, head:var], (accArg2):[head:var, noun:v[acc]])
      case 'по':
        Variable verb = new Variable()
        return u(preposition(prep:'по', head:verb.lightVar),
                poDat(noun:var), dat(noun:var.lightVar, head:var),
                verbalModifier(head:verb.lightVar, hasModifier:true))
      case 'со':
      case 'с':
        return u(preposition(prep:'с')) +
               u(sInstr(noun:var), instr(noun:var.lightVar, head:var)).xor(sGen(noun:var), gen(noun:var.lightVar, head:var))
      case 'о': return uPreposition(var, 'о', oPrep, prep, false)
      case 'к': return uPreposition(var, 'к', kDat, dat)
      case 'у': return uPreposition(var, 'у', uGen, gen)
      case 'до': return uPreposition(var, 'до', doGen, gen)
      case 'от': return uPreposition(var, 'от', otGen, gen)
      case 'в':
        return u(preposition(prep:'в')) +
               u(vAcc(noun:var), acc(noun:var.lightVar, head:var)).xor(vPrep(noun:var), prep(noun:var.lightVar, head:var))
      case 'из':
      case 'изо': return uPreposition(var, 'из', izGen, gen)
      case 'на':
        return uPreposition(var, 'на', naPrep, prep)
      case "и": return u(seq(conj:'and'))
      case "или": return u(seq(conj:'or'))
    }
    return null
  }

  private static Update whWord(Variable var, Variable comp, boolean animate) {
    def attrs = [type:'wh']
    if (animate) {
      attrs['animate'] = 'true'
    }
    return u(filler([:]), question(questioned:var, frame:comp), modality(questioned:var)) + uv(attrs, var)
  }

  static Update uPreposition(Variable var, String prep, Construction prepCxt, Construction caseCxt, boolean withCopula = true) {
    def verb = new Variable()
    def copulaVariant = u(prepCxt(noun:var, head:verb), verbHolder(head:verb), sentenceHolder(head:verb), copula(head:verb), adverb(head:verb)) +
                       Verbs.unomArg(verb) +
                       u(complementizer(content:verb)).xor(question(content:verb))
    def justPrep = u(prepCxt(noun:var))
    return u(preposition(prep:prep), caseCxt(noun:var.lightVar, head:var)) +
           (withCopula ? justPrep.xor(copulaVariant) : justPrep)
  }

  static Update uadv(String rel, String adv) {
    return u(adverb(adv:adv, attr:rel, head:new Variable().lightVar))
  }

  static Update uposs(Variable head, Variable arg = new Variable(), Construction sem) {
    u(possessive(head:head, possessor:arg.lightVar), sem(head:head, possessor:arg))
  }
  static Update uarg(Variable head, Variable arg = new Variable(), Construction syn, Construction... sem) {
    def tokens = new Tokens()
    List<Mite> mites = [syn(noun:arg.lightVar, head:head)]
    for (cxt in sem) {
      mites << cxt(head:head, noun:arg, xor:tokens.a)
    }
    new Update(mites)
  }

  static Update adj(Construction caze, String rel, String val) {
    def noun = new Variable().lightVar
    return u(adjective(nounFrame:noun, rel:rel, val:val), caze(noun:noun))
  }

  static Update numeral(Tokens t, String number) {
    def noun = new Variable()
    def num = new Variable()
    return u(nom(noun:noun, xor:t.a), acc(noun:noun, xor:t.a),
                    (typeCxt(number))(xor:t.bd, var:noun),
                    numQuantifier(num:num, xor:t.b, noun:noun.lightVar, outerNoun:noun,
                            innerCase:(number == '1' ? acc : gen)),
                    (typeCxt(number))(xor:t.d, var:num))
  }

  private static Construction numberTypeCxt(Variable var, String word) {
    return cxt("sem_number_${var}_$word") { ParsingState st, Map args -> st.assign(args.var, 'type', word).assign(args.var, 'number', 'true') }
  }

  private static Update infinitive(Map<Construction, Map> args, Variable verb, String type) {
    def mod = new Variable()
    def datNoun = new Variable().lightVar
    Tokens t = new Tokens()
    return u() + args +
            [(typeCxt(type)):[var:verb], (dat):[noun:datNoun], (datArg1):[head:verb, noun:datNoun], (control): [slave: verb, xor:t.ab],
                    (question): [content:mod, xor:t.a], (modality):[modality:mod, infinitive:verb], (timedModality):[modality:mod],
                    (verbHolder):[head:verb], (sentenceHolder):[head:verb, xor:t.b]]
  }

  private static Update unoun(Map<Construction, Map> mites = [:], Construction caze, Variable noun, String type) {
    def update = upronoun(mites, caze, noun, type)
    update + [(quotedName):[noun:noun]]
  }
  private static Update upronoun(Map<Construction, Map> mites = [:], Construction caze, Variable var, String type) {
    Update update = u()
    if (type) {
      update = update.addCxt(typeCxt(type), var:var)
    }
    return update + mites + commonCase(caze, var)
  }

  private static Update commonCase(Construction caze, Variable var) {
    u(caze(noun:var, xor:new Tokens().a))
  }

  private static Construction typeCxt(String type) {
    return varCxt(type:type)
  }

  static Update uv(Map<String, String> attrs, Variable var) {
    Construction cxt = varCxt(attrs)
    return u(cxt(var:var))
  }

  static Construction varCxt(Map<String, ? extends Object> attrs) {
    return cxt("var_${attrs.collect { k, v -> k + "_" + v}.join("_")}") { st, a ->
      for (attr in attrs.keySet()) {
        st = st.assign(a.var, attr, attrs[attr])
      }
      st
    }
  }

  static Update u(Mite... mites) { new Update(mites) }

}