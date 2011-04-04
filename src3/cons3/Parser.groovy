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
      if (args.hasNoun) {
        state = state.satisfied(acc)
      }
    }
    return handleCase(acc, state, args)
  }
  Construction gen = cxt('gen') { ParsingState state, Map args ->
    if (args.head?.frame(state.chart)?.type && args.noun) {
      state = state.assign(args.head, 'arg1', args.noun)
    }
    return handleCase(gen, state, args)
  }
  Construction instr = cxt('instr') { ParsingState state, Map args -> handleCase(instr, state, args) }
  Construction dat = cxt('dat') { ParsingState state, Map args -> handleCase(dat, state, args) }
  Construction prep = cxt('prep') { ParsingState state, Map args -> handleCase(prep, state, args) }
  Construction sInstr = cxt('sInstr') { ParsingState state, Map args ->
    args.noun ? state.assign(args.head, 'experiencer', args.noun) : state
  }
  Construction poDat = cxt('poDat') { ParsingState state, Map args ->
    args.noun && args.head ? state.assign(args.head, 'topic', args.noun) : state
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
  Construction question = cxt('question') { ParsingState state, Map args ->
    if (args.hasComma && !args.comp && args.head) {
      def next = args.comp ?: new Situation()
      state = state.assign(args.head, args.head.frame(state.chart).type == 'FORGET' ? 'theme' : 'question', next).withSituation(next).apply(question, comp:next)
    }
    if (args.questioned && args.comp) {
      state = state.assign(args.comp, 'questioned', args.questioned)
    }
    return state
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
    return args.head?.frame(state.chart)?.type ? state.assign(args.head, 'arg1', args.possessor) : state
  }
  Construction control = cxt('control') { ParsingState state, Map args ->
    return args.slave ? state.assign(args.head, 'theme', args.slave) : state
  }
  Construction declComp = cxt('declComp') { ParsingState state, Map args ->
    if (args.hasComma && !args.comp && args.head) {
      def next = new Situation()
      state = state.assign(args.head, 'theme', next).withSituation(next).apply(declComp, comp:next)
    }
    return state
  }
  Construction negation = cxt('negation') { ParsingState state, Map args ->
    return state
  }
  Construction also = cxt('also') { ParsingState state, Map args ->
    return state
  }
  Construction vAcc = cxt('vAcc') { ParsingState state, Map args ->
    args.noun ? state.assign(args.head, 'goal', args.noun) : state
  }
  Construction izGen = cxt('izGen') { ParsingState state, Map args ->
    args.noun ? state.assign(args.head, 'source', args.noun) : state
  }
  Construction seq = cxt('seq') { ParsingState state, Map args ->
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
    if (args.noun && args.hasComma && args.wh) {
      def next = new Situation()
      return state.assign(args.noun, 'relative', next).withSituation(next).assign(next, 'wh', args.wh).apply(nestedClause, save: args.save, parent:args.parentSituation)
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

  private ParsingState merge(ParsingState state, Construction cxt, Map oldArgs, Map newArgs, Closure init) {
    if (cxt in [acc, gen] && oldArgs.noun && newArgs.noun) {
      return merge(state, cxt, oldArgs, newArgs, 'noun', init)
    }
    if (cxt == possessive && oldArgs.possessor && newArgs.possessor) {
      return merge(state, cxt, oldArgs, newArgs, 'possessor', init)
    }

    return state.addCtx(newArgs, cxt, init)
  }

  private ParsingState merge(ParsingState state, Construction cxt, Map oldArgs, Map newArgs, String prop, Closure init) {
    Variable oldNoun = oldArgs[prop]
    Frame frame = oldNoun.frame(state.chart)
    if (frame.f('member')) {
      return joinSeq(newArgs, state, cxt, oldNoun, oldArgs, prop, init)
    }
    def multi = new Variable()
    return state.addCtx(oldArgs + [(prop): multi], cxt) { ParsingState st ->
      if (state[seq].conj) {
        st = st.assign(multi, 'conj', state[seq].conj) //todo assign conj in one place
      }
      if (init) st = init(st)
      st.assign(multi, 'member', oldNoun).assign(multi, 'member', newArgs[prop])
    }
  }

  ParsingState joinSeq(Map newArgs, ParsingState state, Construction cxt, Variable seqVar, Map oldArgs, String property, Closure init = null) {
    return state.addCtx(oldArgs + [(property): seqVar], cxt) { ParsingState st ->
      if (state[seq]?.conj) {
        st = st.assign(seqVar, 'conj', state[seq].conj)
      }
      if (init) st = init(st)
      st.assign(seqVar, 'member', newArgs[property])
    }
  }

  private ParsingState addCtx(Map args, ParsingState state, Construction cxt, Closure init = null) {
    if (state[seq]?.hasComma || state[seq]?.conj) {
      def old = state.constructions[cxt]
      if (old) {
        return merge(state, cxt, old, args, init).satisfied(seq)
      }
      old = state[seq].save[cxt]
      if (old) {
        return merge(state, cxt, old, args, init).satisfied(seq)
      }
    }

    return state.addCtx(args, cxt, init)
  }

  ParsingState handleWord(String word, ParsingState state) {
    try {
      Integer.parseInt(word)   //todo generic noun treatment

      def noun = state.newVariable()
      def init = { it.assign(noun, 'type', word).assign(noun, 'number', 'true') }

      state = addCtx(state, acc, noun:noun, hasNoun:true, init)

      def save = state.constructions

      state = state.applyAll(acc)

      def qv = state[questionVariants]
      if (qv) {
        def seqVar = state.newVariable()
        state = state.apply(questionVariants, seq:seqVar).satisfied(questionVariants)
        state = joinSeq(state, acc, seqVar, noun:noun, [hasNoun:true], 'noun', init).applyAll(acc)
      }


      state = state.apply(seq, save:save)

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
        def noun = state.newVariable()
        def init = { it.assign(noun, 'type', 'STREET') }
        state = state.apply(adjective, nounFrame:noun, rel:'name', val:word[0..-3]+"ая", init)

        state = addCtx(state, gen, noun:noun, init)

        def save = state.constructions
        state = state.applyAll(gen)
        state = state.apply(seq, save:save)
        return state
      case "коммерческий":
        def noun = state[acc]?.noun ?: state.newVariable()
        return state.apply(adjective, nounFrame:noun, rel:'kind', val:'COMMERCIAL').apply(acc, noun:noun)
      case "маленький":
        def noun = state[acc]?.noun ?: state.newVariable()
        return state.apply(adjective, nounFrame:noun, rel:'size', val:'LITTLE').apply(acc, noun:noun)
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
        return state.apply(declComp, head:state[nom].noun)
      case "поводу":
        return noun(state, dat) { st, noun -> st.assign(noun, 'type', 'MATTER') }
      case "недоумении":
        return noun(state, prep) { st, noun -> st.assign(noun, 'type', 'PREDICAMENT') }
      case "рта":
        return noun(state, gen) { st, noun -> st.assign(noun, 'type', 'MOUTH') }
      case "молоточек":
        return noun(state, acc) { st, noun -> st.assign(noun, 'type', 'HAMMER') }
      case "улицы":
        return noun(state, gen) { st, noun -> st.assign(noun, 'type', 'STREET') }
      case "углу":  //todo plain noun
        def noun = state.newVariable()
        return state.apply(atCorner, noun:noun).apply(gen, head:noun)
      case "магазин":
        return noun(state, acc) { st, noun -> st.assign(noun, 'type', 'SHOP').assign(noun, 'given', 'false') }
      case "случился":
        Variable verb = state.newVariable()
        state = state.assign(verb, 'type', 'HAPPEN').assign(situation, 'time', 'PAST')
        return state.apply(sInstr, head:verb).apply(nom, head:verb)
      case 'со': return preposition(state, sInstr, instr)
      case 'по': return preposition(state, poDat, dat)
      case 'о': return preposition(state, oPrep, prep)
      case 'к': return preposition(state, kDat, dat)
      case 'в': return preposition(state, vAcc, acc)
      case 'изо': return preposition(state, izGen, gen)
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
        state = addCtx(state, possessive, possessor:me) { it.assign(me, 'type', 'ME') }
        def save = state.constructions
        return state.applyAll(possessive).apply(seq, save:save)
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
        def init = { st -> st.assign(they, 'type', 'THEY') }
        state = addCtx(state, possessive, possessor:they, init)
        state = addCtx(state, acc, noun:they, hasNoun:true, init)
        def save = state.constructions
        return state.applyAll(acc, possessive).apply(seq, save:save)
      case "Они":
      case "они":
        return noun(state, nom) { st, noun -> st.assign(noun, 'type', 'THEY') }
      case "соседям": return noun(state, dat) { st, noun -> st.assign(noun, 'type', 'NEIGHBOURS') }
      case "кассиршу": return noun(state, acc) { st, noun -> st.assign(noun, 'type', 'CASHIER').assign(noun, 'given', 'false') }
      case "Кассирша": return noun(state, nom) { st, noun -> st.assign(noun, 'type', 'CASHIER') }
      case "порядок":
        def noun = state.newVariable()
        state = state.apply(acc, noun: noun, hasNoun:true) { it.assign(noun, 'type', 'ORDER') }
        return state.apply(nounGen, head:noun) //todo one noun frame - several cases
      case "счета":
        return noun(state, nounGen) { st, noun -> st.assign(noun, 'type', 'COUNTING') }
      case "вдруг":
        if (state[nom]) {
          def verb = state.newVariable()
          return state.assign(verb, 'manner', 'SUDDENLY').apply(nom, head:verb)
        }
        return state
      case "грустно":
        if (state[nom]) {
          def verb = state.newVariable()
          return state.assign(verb, 'manner', 'SADLY').apply(nom, head:verb)
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
          return state.apply(question, head:verb).apply(nom, head:verb)
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
          state = state.assign(verb, 'type', 'DISCOVER').assign(situation, 'time', 'PAST').inhibit(relativeClause) //todo generic suppress for context-freeness
          return state.satisfied(declComp).apply(declComp, head:verb).apply(nom, head:verb)
        }
        return state
      case "улыбнулась":
        if (state[nom]) {
          def verb = state[nom].head
          state = state.assign(verb, 'type', 'SMILE').assign(situation, 'time', 'PAST')
          return state.apply(nom, head:verb)
        }
        return state
      case "вынула":
        if (state[nom]) {
          def verb = state.newVariable()
          state = state.assign(verb, 'type', 'TAKE_OUT').assign(situation, 'time', 'PAST')
          return state.apply(nom, head:verb, noun:state[nom].noun).apply(izGen, head:verb).apply(acc, head:verb)
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
          return state.apply(acc, head:verb).apply(args, nom).apply(question, head:verb).apply(oPrep, head:verb)
        }
        return state
      case ",":
        state = state.apply(declComp, hasComma:true).apply(question, hasComma:true).apply(relativeClause, hasComma:true, parentSituation:situation)
        if (state[seq]) {
          state = state.apply(seq, hasComma:true)
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
        if (state[relativeClause]?.hasComma) {
          def wh = new Variable()
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
      state = state.apply(possessive, head:noun)
    }

    state = state.apply(quotedName, noun:noun).satisfied(relativeClause).apply(relativeClause, noun:noun, save:state.constructions)

    return state
  }

  private ParsingState preposition(ParsingState state, Construction prepCtx, Construction caze) {
    def noun = state.newVariable()
    state = state.apply(prepCtx, noun:noun)
    def save = state.constructions
    return state.clearConstructions().apply(caze, save: save, delegate: prepCtx, noun:noun)
  }

}