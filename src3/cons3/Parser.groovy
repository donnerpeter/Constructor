package cons3

/**
 * @author peter
 */
class Parser {

  Chart parse(String text, debug = false) {
    ParsingState state = new ParsingState(chart: new Chart(), situation: new Situation(), participants:[:], constructions:[:])
    def tokenizer = new StringTokenizer(text, """ '":,.""", true)
    for (String w in tokenizer) {
      if (w != ' ') {
        state = handleWord(w, state)
        if (debug) {
          println w
          state.constructions.each {
            println "  $it.key -> $it.value"
          }
        }
      }
    }
    return state.chart
  }

  Construction cxt(String name, Closure cl) {
    new Construction(name, cl as Function2)
  }

  private static ParsingState handleCase(Construction caze, ParsingState state, Map args) {
    if (args.save && args.hasNoun) {
      return state.satisfied(caze).restore(args.save).apply(args.delegate, noun: args.noun)
    }
    return state
  }

  Construction adjective = cxt('adjective') { ParsingState state, Map args -> state.assign(args.nounFrame, args.rel, args.val) }
  Construction nom = cxt('nom') { ParsingState state, Map args ->
    args.head?.frame(state.chart)?.type ? state.assign(args.head, 'arg1', args.noun) : state
  }
  Construction acc = cxt('acc') { ParsingState state, Map args ->
    if (args.head?.frame(state.chart)?.type && args.noun) {
      state = state.assign(args.head, 'arg2', args.noun)
    }
    return handleCase(acc, state, args)
  }
  Construction gen = cxt('gen') { ParsingState state, Map args ->
    args.head?.frame(state.chart)?.type && args.noun ? state.assign(args.head, 'arg1', args.noun) : state
  }
  Construction instr = cxt('instr') { ParsingState state, Map args -> handleCase(instr, state, args) }
  Construction dat = cxt('dat') { ParsingState state, Map args -> handleCase(dat, state, args) }
  Construction prep = cxt('prep') { ParsingState state, Map args -> handleCase(prep, state, args) }
  Construction sInstr = cxt('sInstr') { ParsingState state, Map args ->
    args.noun ? state.assign(args.head, 'experiencer', args.noun) : state
  }
  Construction poDat = cxt('poDat') { ParsingState state, Map args ->
    args.noun ? state.assign(args.head, 'topic', args.noun) : state
  }
  Construction oPrep = cxt('oPrep') { ParsingState state, Map args ->
    args.noun ? state.assign(args.head, 'topic', args.noun) : state
  }
  Construction nounGen = cxt('nounGen') { ParsingState state, Map args -> //todo nounGen -> gen
    args.noun ? state.assign(args.head, 'criterion', args.noun) : state
  }
  Construction kDat = cxt('kDat') { ParsingState state, Map args ->
    args.noun ? state.assign(args.head, 'goal', args.noun) : state
  }
  Construction comp = cxt('comp') { ParsingState state, Map args ->
    args.comp ? state.assign(args.head, args.head.frame(state.chart).type in ['FORGET', 'DISCOVER', 'AMAZE'] ? 'theme' : 'question', args.comp) : state
  }
  Construction question = cxt('question') { ParsingState state, Map args ->
    args.questioned ? state.assign(args.situation, 'questioned', args.questioned) : state
  }
  Construction comeScalarly = cxt('comeScalarly') { ParsingState state, Map args ->
    args.order ? state.assign(args.verb, 'type', 'COME_SCALARLY').assign(args.verb, 'order', args.order) : state
  }
  Construction questionVariants = cxt('questionVariants') { ParsingState state, Map args ->
    args.seq ? state.assign(args.questioned, 'variants', args.seq) : state
  }
  Construction shortAdjCopula = cxt('shortAdjCopula') { ParsingState state, Map args ->
    if (args.noun && args.pred) {
      state = state.assign(args.noun, 'degree', args.pred)
    }
    return args.time ? state.assign(args.situation, 'time', args.time) : state
  }
  Construction possessive = cxt('possessive') { ParsingState state, Map args ->
    state = args.conj ? possessive.action.call(state, args.conj) : state //todo generic conj handling
    return args.head.frame(state.chart).type ? state.assign(args.head, 'arg1', args.possessor) : state
  }
  Construction control = cxt('control') { ParsingState state, Map args ->
    return args.slave ? state.assign(args.head, 'theme', args.slave) : state
  }
  Construction declComp = cxt('declComp') { ParsingState state, Map args ->
    return state
  }
  Construction negation = cxt('negation') { ParsingState state, Map args ->
    return state
  }
  Construction also = cxt('also') { ParsingState state, Map args ->
    return state
  }
  Construction vAcc = cxt('vAcc') { ParsingState state, Map args ->
    return state
  }
  Construction seq = cxt('seq') { ParsingState state, Map args ->
    if (args.seq && args.member) {
      state = state.assign(args.seq, 'member', args.member)

      state.chart.allAssignments(state.situation).each {
        if (it.frame.var != args.seq && it.value instanceof Frame && it.value.var == args.member) {
          state = state.assign(it.frame.var, it.property, args.seq)
        }
      }

      if (args.conj) {
        state = state.assign(args.seq, 'conj', args.conj)
      }
    }
    return state
  }
  Construction advObj = cxt('advObj') { ParsingState state, Map args ->
    if (args.head && args.adv) {
      state = state.assign(args.head, 'arg2', args.adv)
    }
    return state
  }
  Construction quotedName = cxt('quotedName') { ParsingState state, Map args ->
    if (args.finished) {
      state = state.assign(args.noun, 'name', args.name).satisfied(quotedName)
    }
    return state
  }
  Construction relativeClause = cxt('relativeClause') { ParsingState state, Map args ->
    if (args.noun && args.clause && args.wh) {
      return state.assign(args.noun, 'relative', args.clause).withSituation(args.clause).assign(args.clause, 'wh', args.wh).apply(nestedClause, save: args.save, parent:args.noun.situation)
    }
    return state
  }
  Construction nestedClause = cxt('nestedClause') { ParsingState state, Map args ->
    return state
  }
  Construction atCorner = cxt('atCorner') { ParsingState state, Map args ->
    if (args.noun) {
      def corner = args.noun
      state = state.assign(corner, 'type', 'CORNER')
      return state.assign(args.head, 'location', corner)
    }
    return state
  }

  ParsingState handleWord(String word, ParsingState state) {
    try {
      Integer.parseInt(word)   //todo generic noun treatment
      def noun = state.newVariable()
      def init = { it.assign(noun, 'type', word).assign(noun, 'number', 'true') }
      state = state.apply(acc, noun:noun, hasNoun:noun, init)

      def seqVar = null
      if (state[seq]?.hasComma || state[seq]?.conj) {
        seqVar = state[seq].seq ?: state.newVariable()
        state = state.apply(seq, seq:seqVar).satisfied(seq)
      }
      def qv = state[questionVariants]
      if (qv) {
        assert !seqVar
        seqVar = state.newVariable()
        state = state.apply(questionVariants, seq:seqVar).satisfied(questionVariants)
      }

      if (seqVar) {
        state = state.apply(acc, noun:seqVar, hasNoun:true)
      }

      state = state.apply(seq, member:noun, seq:seqVar, init)

      return state
    } catch (NumberFormatException e) {
    }

    if (state[quotedName]?.started && !state[quotedName].name) {
      state = state.apply(quotedName, name:word)
    }

    def situation = state.situation
    switch (word) {
      case "Удивительный":
        def noun = state.newVariable()
        return state.apply(adjective, nounFrame:noun, rel:'property', val:'AMAZING').apply(nom, noun:noun)
      case "Знаменской": // todo a unified treatment for street names
      case "Бассейной":
        def seqVar = null
        if (state[seq]?.hasComma || state[seq]?.conj) {
          seqVar = state[seq].seq ?: state.newVariable()
          state = state.apply(seq, seq:seqVar).satisfied(seq)
        }

        def noun = state.newVariable()
        def init = { it.assign(noun, 'type', 'STREET') }
        return state.apply(adjective, nounFrame:noun, rel:'name', val:word[0..-3]+"ая", init).apply(gen, noun:noun, init).apply(seq, member:noun, seq:seqVar, init)
      case "коммерческий":
        def noun = state[acc]?.noun ?: state.newVariable()
        return state.apply(adjective, nounFrame:noun, rel:'kind', val:'COMMERCIAL').apply(acc, noun:noun)
      case "нашем":
        def we = state.newVariable()
        def possHead = state[prep]?.noun ?: state.newVariable()
        state = state.assign(we, 'type', 'WE')
        return state.apply(possessive, possessor:we, head:possHead)
      case "этому":
        def noun = state[dat]?.noun ?: state.newVariable()
        return state.apply(adjective, nounFrame:noun, rel:'determiner', val:'THIS').apply(dat, noun:noun)
      case "случай": return noun(state, nom) { st, noun -> st.assign(noun, 'type', 'THING').assign(noun, 'given', 'false') }
      case "удивление":
        state = noun(state, nom) { st, noun -> st.assign(noun, 'type', 'AMAZE') }
        return state.apply(comp, head:state[nom].noun)
      case "поводу":
        return noun(state, dat) { st, noun -> st.assign(noun, 'type', 'MATTER') }
      case "недоумении":
        return noun(state, prep) { st, noun -> st.assign(noun, 'type', 'PREDICAMENT') }
      case "улицы":
        return noun(state, gen) { st, noun -> st.assign(noun, 'type', 'STREET') }
      case "углу":  //todo plain noun
        def noun = state.newVariable()
        return state.apply(atCorner, noun:noun).apply(gen, head:noun)
      case "магазин":
        return noun(state, acc) { st, noun -> st.assign(noun, 'type', 'SHOP') }
      case "случился":
        Variable verb = state.newVariable()
        state = state.assign(verb, 'type', 'HAPPEN').assign(situation, 'time', 'PAST')
        return state.apply(sInstr, head:verb).apply(nom, head:verb)
      case 'со': return preposition(state, sInstr, instr)
      case 'по': return preposition(state, poDat, dat)
      case 'о': return preposition(state, oPrep, prep)
      case 'к': return preposition(state, kDat, dat)
      case 'в': return preposition(state, vAcc, acc)
      case 'на': return state.apply(atCorner)
      case "мной": return noun(state, instr) { st, noun -> st.assign(noun, 'type', 'ME') }
      case ":":
        def elaboration = new Situation()
        state = state.assign(situation, 'elaboration', elaboration)
        return state.withSituation(elaboration)
      case "я":
      case "Я": return noun(state, nom) { st, noun -> st.assign(noun, 'type', 'ME') }
      case "Мы": return noun(state, nom) { st, noun -> st.assign(noun, 'type', 'WE') }
      case "мое":
        def me = state.newVariable()
        def poss = state[possessive]
        def possHead = !state[nom]?.hasNoun && state[nom]?.noun ? state[nom].noun : state.newVariable()
        state = state.assign(me, 'type', 'ME')
        if (poss) {
          state = state.satisfied(possessive)
        }
        return state.apply(possessive, possessor:me, head:possHead, conj: poss)
      case "и": return state[seq] ? state.apply(seq, conj:'and') : state
      case "или": return state[seq] ? state.apply(seq, conj:'or') : state
      case "а":
        def next = new Situation()
        return state.assign(situation, 'but', next).withSituation(next)
      case "все":
        return state.assign(state[nom].noun, 'quantifier', 'ALL')
      case "дальше":
        def adv = state.newVariable()
        return state.apply(advObj, adv: adv) { it.assign(adv, 'type', 'NEXT') }
      case "их":
        def they = state.newVariable()
        def verb = state[acc]?.head ?: state.newVariable()
        def possHead = !state[nom]?.hasNoun && state[nom]?.noun ? state[nom].noun : state.newVariable()

        def init = { st -> st.assign(they, 'type', 'THEY') }

        return state.
                addCtx(acc, noun:they, head:verb, init).
                addCtx(possessive, possessor:they, head:possHead, init).
                applyAll(acc, possessive)
      case "Они":
      case "они":
        return noun(state, nom) { st, noun -> st.assign(noun, 'type', 'THEY') }
      case "соседям": return noun(state, dat) { st, noun -> st.assign(noun, 'type', 'NEIGHBOURS') }
      case "кассиршу": return noun(state, acc) { st, noun -> st.assign(noun, 'type', 'CASHIER') }
      case "порядок":
        state = noun(state, acc) { st, noun -> st.assign(noun, 'type', 'ORDER') }
        return state.apply(nounGen, head:state[acc].noun)
      case "счета":
        return noun(state, nounGen) { st, noun -> st.assign(noun, 'type', 'COUNTING') }
      case "вдруг":
        if (state[nom]) {
          def verb = state.newVariable()
          return state.assign(verb, 'manner', 'SUDDENLY').apply(nom, head:verb)
        }
        return state
      case "тоже":
        def alsoVar = state.newVariable()
        def subj = state.newVariable()
        state = state.assign(alsoVar, 'type', 'ALSO')
        state = state.assign(alsoVar, 'arg1', subj)
        return state.apply(also, also:alsoVar, subj:subj)
      case "не":
        return state.apply(negation)
      case "забыл":
        if (state[nom]) {
          Variable verb = state[nom].head ?: state.newVariable()
          state = state.assign(verb, 'type', 'FORGET').assign(situation, 'time', 'PAST')
          return state.apply(comp, head:verb).apply(nom, head:verb)
        }
        return state
      case "забыли":
        Variable verb = state.newVariable()
        state = state.assign(verb, 'type', 'FORGET').assign(situation, 'time', 'PAST')
        def subj = state.newVariable()
        return state.apply(advObj, head:verb).apply(acc, head:verb).apply(nom, noun:subj, head:verb) { it.assign(subj, 'type', 'THEY') }
      case "помнят":
        Variable verb = state.newVariable()
        state = state.assign(verb, 'type', 'REMEMBER').assign(situation, 'time', 'PRESENT')
        state = state.apply(acc, head:verb)
        if (!state[nom]) {
          def subj = state.newVariable()
          state = state.apply(nom, noun:subj) { it.assign(subj, 'type', 'THEY') }
        }
        return state.apply(nom, head:verb)
      case "могут":
        def verb = state.newVariable()
        def also = state[also]
        def subj = null
        if (also) {
          subj = also.subj
          state = state.assign(also.also, 'theme', verb)
        }
        if (state[negation] != null) {
          state = state.assign(verb, 'negated', 'true')
        }
        if (!subj) {
          subj = state.newVariable()
        }
        state = state.assign(verb, 'type', 'CAN').assign(situation, 'time', 'PRESENT')
        state = state.assign(subj, 'type', 'THEY')
        return state.apply(control, subj:subj, head:verb)
      case "отправился":
        if (state[nom]) {
          def verb = state.newVariable()
          state = state.assign(verb, 'type', 'GO_OFF').assign(situation, 'time', 'PAST')
          return state.apply(kDat, head:verb).apply(nom, head:verb)
        }
        return state
      case "пошли":
        if (state[nom]) {
          def verb = state.newVariable()
          state = state.assign(verb, 'type', 'GO').assign(situation, 'time', 'PAST')
          return state.apply(vAcc, head:verb).apply(nom, head:verb)
        }
        return state
      case "обнаружили":
        if (state[nom]) {
          def verb = state[nom].head
          state = state.assign(verb, 'type', 'DISCOVER').assign(situation, 'time', 'PAST')
          return state.apply(comp, head:verb).apply(nom, head:verb)
        }
        return state
      case "вспомнить":
        def verb = state.newVariable()
        state = state.apply(control, slave:verb)
        return state.assign(verb, 'type', 'RECALL').apply(acc, head:verb)
      case "думают":
        if (state[nom]) {
          def verb = state.newVariable()
          state = state.assign(verb, 'type', 'THINK').assign(situation, 'time', 'PRESENT')
          return state.apply(poDat, head:verb).apply(nom, head:verb).apply(acc, head:verb)
        }
        return state
      case "спросил":
      case "спросили":
        def args = state[nom]
        if (args) {
          state = state.satisfied(nom)
          def verb = state.newVariable()
          args = args + [head:verb]
          state = state.assign(verb, 'type', 'ASK').assign(situation, 'time', 'PAST')
          return state.apply(acc, head:verb).apply(args, nom).apply(comp, head:verb).apply(oPrep, head:verb)
        }
        return state
      case ",":
        def next = new Situation()
        if (state[comp]) {
          state = state.apply(comp, comp:next).withSituation(next).apply(state[comp].head.frame(state.chart).type in ['DISCOVER', 'AMAZE'] ? declComp : question, situation:next)
        }
        if (state[seq]) {
          state = state.apply(seq, hasComma:true)
        }
        if (state[relativeClause]) {
          state = state.apply(relativeClause, clause:next)
        }
        if (state[nestedClause]) {
          state = state.withSituation(state[nestedClause].parent).clearConstructions().restore(state[nestedClause].save).satisfied(nestedClause)
        }
        return state
      case "что":
        def noun = state.newVariable()
        if (state[question]) {
          state = state.apply(question, questioned:noun)
        }
        if (!state[declComp]) {
          //todo generic noun treatment for что
          state = state.
                  addCtx(nom, noun:noun, hasNoun:'true').
                  addCtx(acc, noun:noun, hasNoun:'true').
                  applyAll(nom, acc)
        }
        if (state[relativeClause]?.clause) {
          def wh = new Variable(state[relativeClause].clause)
          state = state.apply(relativeClause, wh:wh).apply(atCorner, head:wh) //todo pp copula
        }
        return state
      case "идет":
      case "идёт":
        Variable verb = state.newVariable()
        state = state.assign(situation, 'time', 'PRESENT')
        return state.apply(nom, head:verb).apply(comeScalarly, verb:verb)
      case "раньше":
        def cs = state[comeScalarly]
        if (cs) {
          return state.apply(comeScalarly, order:'EARLIER').apply(nom)
        }
        return state
      case "-":
        if (state[question]) {
          return state.apply(questionVariants, questioned:state[question].questioned)
        }
        return state
      case ".":
        return state.assign(state.situation, 'dot', 'true').withSituation(new Situation())
      case 'Каково':
        def degree = state.newVariable()
        state = state.assign(situation, 'exclamation', degree)
        return state.apply(shortAdjCopula, pred:degree, situation:state.situation)
      case 'было':
        def noun = state.newVariable()
        return state.apply(shortAdjCopula, time:'PAST', noun: noun).apply(nom, noun:noun)
      case '"':
        if (state[quotedName]) {
          if (state[quotedName].started) {
            state = state.apply(quotedName, finished:true)
          } else {
            state = state.apply(quotedName, started:true)
          }
        }
        return state
    }
    return state
  }

  private ParsingState noun(ParsingState state, Construction caze, Closure init) {
    if (state.constructions[caze]?.hasNoun) {
      state = state.inhibit(caze)
    }

    def noun = state.constructions[caze]?.noun ?: state.newVariable()
    if (caze == nom) {
      state = state.apply(shortAdjCopula, noun:noun)
    }

    state = state.apply(caze, noun: noun, hasNoun:'true') { init(it, noun) }
    if (state[possessive]) {
      state = state.apply(possessive)
    }

    state = state.apply(quotedName, noun:noun).apply(relativeClause, noun:noun, save:state.constructions)

    return state
  }

  private ParsingState preposition(ParsingState state, Construction prepCtx, Construction caze) {
    def noun = state.newVariable()
    state = state.apply(prepCtx, noun:noun)
    def save = state.constructions
    return state.clearConstructions().apply(caze, save: save, delegate: prepCtx, noun:noun)
  }

}