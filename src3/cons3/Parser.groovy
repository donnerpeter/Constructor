package cons3

/**
 * @author peter
 */
class Parser {

  Chart parse(String text) {
    ParsingState state = new ParsingState(chart: new Chart(), situation: new Situation(), participants:[:], constructions:[:])
    def tokenizer = new StringTokenizer(text, """ '":,.""", true)
    for (String w in tokenizer) {
      if (w != ' ') {
        state = handleWord(w, state)
      }
    }
    return state.chart
  }

  ParsingState handleWord(String word, ParsingState state) {
    try {
      Integer.parseInt(word)   //todo generic noun treatment
      def noun = state.newFrame()
      def init = { it.assign(noun, 'type', word).assign(noun, 'number', 'true') }
      state = state.apply('nom', noun:noun, hasNoun:noun, init)

      def seqVar = null
      if (state.constructions.seq?.hasComma || state.constructions.seq?.conj) {
        seqVar = state.constructions.seq.seq ?: state.newFrame()
        state = state.apply('seq', seq:seqVar).satisfied('seq')
      }
      def qv = state.constructions.questionVariants
      if (qv) {
        assert !seqVar
        seqVar = state.newFrame()
        state = state.apply('questionVariants', seq:seqVar).satisfied('questionVariants')
      }

      state = state.apply('seq', member:noun, seq:seqVar, init).apply('acc', noun:seqVar, hasNoun:true)

      return state
    } catch (NumberFormatException e) {
    }

    def situation = state.situation
    switch (word) {
      case "Удивительный":
        def noun = state.newFrame()
        return state.apply('adjective', nounFrame:noun, rel:'property', val:'AMAZING').apply('nom', noun:noun)
      case "этому":
        def noun = state.constructions.dat?.noun ?: state.newFrame()
        return state.apply('adjective', nounFrame:noun, rel:'determiner', val:'THIS').apply('dat', noun:noun)
      case "случай": return noun(state, 'nom') { st, noun -> st.assign(noun, 'type', 'THING').assign(noun, 'given', 'false') }
      case "удивление":
        state = noun(state, 'nom') { st, noun -> st.assign(noun, 'type', 'AMAZE') }
        return state.apply('comp', head:state.constructions.nom.noun)
      case "поводу":
        return noun(state, 'dat') { st, noun -> st.assign(noun, 'type', 'MATTER') }
      case "случился":
        Variable verb = state.newFrame()
        state = state.assign(verb, 'type', 'HAPPEN').assign(situation, 'time', 'PAST')
        return state.apply('sInstr', head:verb).apply('nom', head:verb)
      case 'со': return preposition(state, 'sInstr', 'instr')
      case 'по': return preposition(state, 'poDat', 'dat')
      case 'к': return preposition(state, 'kDat', 'dat')
      case "мной": return noun(state, 'instr') { st, noun -> st.assign(noun, 'type', 'ME') }
      case ":":
        def elaboration = new Situation()
        state = state.assign(situation, 'elaboration', elaboration)
        return state.withSituation(elaboration)
      case "я":
      case "Я": return noun(state, 'nom') { st, noun -> st.assign(noun, 'type', 'ME') }
      case "мое":
        def me = state.newFrame()
        def poss = state.constructions.possessive
        def possHead = !state.constructions.nom?.hasNoun && state.constructions.nom?.noun ? state.constructions.nom.noun : state.newFrame()
        state = state.assign(me, 'type', 'ME')
        if (poss) {
          state = state.satisfied('possessive')
        }
        return state.apply('possessive', possessor:me, head:possHead, conj: poss)
      case "и": return state.constructions.seq ? state.apply('seq', conj:'and') : state
      case "или": return state.constructions.seq ? state.apply('seq', conj:'or') : state
      case "а":
        def next = new Situation()
        return state.assign(situation, 'but', next).withSituation(next)
      case "дальше":
        def adv = state.newFrame()
        return state.apply('advObj', adv: adv) { it.assign(adv, 'type', 'NEXT') }
      case "их":
        def they = state.newFrame()
        def verb = state.constructions.acc?.head ?: state.newFrame()
        def possHead = !state.constructions.nom?.hasNoun && state.constructions.nom?.noun ? state.constructions.nom.noun : state.newFrame()

        def init = { st -> st.assign(they, 'type', 'THEY') }

        return state.
                addCtx('acc', noun:they, head:verb, init).
                addCtx('possessive', possessor:they, head:possHead, init).
                applyAll('acc', 'possessive')
      case "они": return noun(state, 'nom') { st, noun -> st.assign(noun, 'type', 'THEY') }
      case "соседям": return noun(state, 'dat') { st, noun -> st.assign(noun, 'type', 'NEIGHBOURS') }
      case "порядок":
        state = noun(state, 'acc') { st, noun -> st.assign(noun, 'type', 'ORDER') }
        return state.apply('nounGen', head:state.constructions.acc.noun)
      case "счета":
        return noun(state, 'nounGen') { st, noun -> st.assign(noun, 'type', 'COUNTING') }
      case "вдруг":
        def nom = state.constructions.nom
        if (nom) {
          def verb = state.newFrame()
          return state.assign(verb, 'manner', 'SUDDENLY').apply('nom', head:verb)
        }
        return state
      case "тоже":
        def also = state.newFrame()
        def subj = state.newFrame()
        state = state.assign(also, 'type', 'ALSO')
        state = state.assign(also, 'arg1', subj)
        return state.apply('also', also:also, subj:subj)
      case "не":
        return state.apply('negation')
      case "забыл":
        def nom = state.constructions.nom
        if (nom) {
          Variable verb = nom.head ?: state.newFrame()
          state = state.assign(verb, 'type', 'FORGET').assign(situation, 'time', 'PAST')
          return state.apply('comp', head:verb).apply('nom', head:verb)
        }
        return state
      case "забыли":
        Variable verb = state.newFrame()
        state = state.assign(verb, 'type', 'FORGET').assign(situation, 'time', 'PAST')
        def subj = state.newFrame()
        return state.apply('advObj', head:verb).apply('nom', noun:subj, head:verb) { it.assign(subj, 'type', 'THEY') }
      case "помнят":
        Variable verb = state.newFrame()
        state = state.assign(verb, 'type', 'REMEMBER').assign(situation, 'time', 'PRESENT')
        def subj = state.newFrame()
        return state.apply('acc', head:verb).apply('nom', noun:subj, head:verb) { it.assign(subj, 'type', 'THEY') }
      case "могут":
        def verb = state.newFrame()
        def also = state.constructions.also
        def subj = null
        if (also) {
          subj = also.subj
          state = state.assign(also.also, 'theme', verb)
        }
        if (state.constructions.negation != null) {
          state = state.assign(verb, 'negated', 'true')
        }
        if (!subj) {
          subj = state.newFrame()
        }
        state = state.assign(verb, 'type', 'CAN').assign(situation, 'time', 'PRESENT')
        state = state.assign(subj, 'type', 'THEY')
        return state.apply('control', subj:subj, head:verb)
      case "отправился":
        def nom = state.constructions.nom
        if (nom) {
          def verb = state.newFrame()
          state = state.assign(verb, 'type', 'GO_OFF').assign(situation, 'time', 'PAST')
          return state.apply('kDat', head:verb).apply('nom', head:verb)
        }
        return state
      case "обнаружили":
        def nom = state.constructions.nom
        if (nom) {
          def verb = nom.head
          state = state.assign(verb, 'type', 'DISCOVER').assign(situation, 'time', 'PAST')
          return state.apply('comp', head:verb).apply('nom', head:verb)
        }
        return state
      case "вспомнить":
        def verb = state.newFrame()
        state = state.apply('control', slave:verb)
        return state.assign(verb, 'type', 'REMEMBER').apply('acc', head:verb)
      case "думают":
        def nom = state.constructions.nom
        if (nom) {
          def verb = state.newFrame()
          state = state.assign(verb, 'type', 'THINK').assign(situation, 'time', 'PRESENT')
          return state.apply('poDat', head:verb).apply('nom', head:verb).apply('acc', head:verb)
        }
        return state
      case "спросил":
        def nom = state.constructions.nom
        if (nom) {
          state = state.satisfied('nom')
          def verb = state.newFrame()
          nom = nom + [head:verb]
          state = state.assign(verb, 'type', 'ASK')//todo don't reassign tense .assign(situation, 'time', 'PAST', false)
          return state.apply('acc', head:verb).apply(nom, 'nom').apply('comp', head:verb)
        }
        return state
      case ",":
        def next = new Situation()
        def comp = state.constructions.comp
        if (comp) {
          return state.apply('comp', comp:next).withSituation(next).apply(comp.head.frame(state.chart).type in ['DISCOVER', 'AMAZE'] ? 'declComp' : 'question', situation:next)
        }
        return state.constructions.seq ? state.apply('seq', hasComma:true) : state
      case "что":
        if (state.constructions.question) {
          def noun = state.newFrame()
          return state.
                  addCtx('nom', noun:noun, hasNoun:'true').
                  addCtx('acc', noun:noun, hasNoun:'true').
                  addCtx('question', questioned:noun).
                  applyAll('nom', 'acc', 'question')
        }
        return state
      case "идет":
      case "идёт":
        Variable verb = state.newFrame()
        state = state.assign(situation, 'time', 'PRESENT')
        return state.apply('nom', head:verb).apply('comeScalarly', verb:verb)
      case "раньше":
        def cs = state.constructions.comeScalarly
        if (cs) {
          return state.apply('comeScalarly', order:'EARLIER').apply('nom')
        }
        return state
      case "-":
        if (state.constructions.question) {
          return state.apply('questionVariants', questioned:state.constructions.question.questioned)
        }
        return state
      case ".":
        return state.assign(state.situation, 'dot', 'true').withSituation(new Situation())
      case 'Каково':
        def degree = state.newFrame()
        state = state.assign(situation, 'exclamation', degree)
        return state.apply('shortAdjCopula', pred:degree, situation:state.situation)
      case 'было':
        def noun = state.newFrame()
        return state.apply('shortAdjCopula', time:'PAST', noun: noun).apply('nom', noun:noun)
    }
    return state
  }

  private ParsingState noun(ParsingState state, String caze, Closure init) {
    if (state.constructions[caze]?.hasNoun) {
      state = state.inhibit(caze)
    }

    def noun = state.constructions[caze]?.noun ?: state.newFrame()
    if (caze == 'nom') {
      state = state.apply('shortAdjCopula', noun:noun)
    }

    state = state.apply(caze, noun: noun, hasNoun:'true') { init(it, noun) }
    if (state.constructions.possessive) {
      state = state.apply('possessive')
    }

    return state
  }

  private ParsingState preposition(ParsingState state, String prepCtx, String caze) {
    def noun = state.newFrame()
    state = state.apply(prepCtx, noun:noun)
    def save = state.constructions
    return state.clearConstructions().apply(caze, save: save, delegate: prepCtx, noun:noun)
  }

}