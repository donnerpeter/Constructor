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
    def situation = state.situation
    switch (word) {
      case "Удивительный":
        def noun = state.newFrame()
        def verb = state.newFrame()
        return state.apply('adjective', nounFrame:noun, rel:'property', val:'AMAZING').apply('nom', noun:noun, head:verb)
      case "этому":
        def noun = state.newFrame()
        return state.apply('adjective', nounFrame:noun, rel:'determiner', val:'THIS').apply('dat', noun:noun)
      case "случай": return noun(state, 'nom') { st, noun -> st.assign(noun, 'type', 'THING').assign(noun, 'given', 'false') }
      case "удивление": //todo noun
        def poss = state.constructions.possessive
        if (poss) {
          return state.assign(poss.head, 'type', 'AMAZE').apply('possessive').apply('comp', head:poss.head)
        }
        return state
      case "поводу": return noun(state, 'dat') { st, noun -> st.assign(noun, 'type', 'MATTER') }
      case "случился":
        def nom = state.constructions.nom
        if (nom) {
          Variable verb = nom.head
          state = state.assign(verb, 'type', 'HAPPEN').assign(situation, 'time', 'PAST')
          return state.apply('sInstr', head:verb).apply('nom')
        }
        return state
      case 'со': return preposition(state, 'sInstr', 'instr')
      case 'по': return preposition(state, 'poDat', 'dat')
      case 'к': return preposition(state, 'kDat', 'dat')
      case "мной": return noun(state, 'instr') { st, noun -> st.assign(noun, 'type', 'ME') }
      case ":":
        def elaboration = new Situation()
        state = state.assign(situation, 'elaboration', elaboration)
        return state.withSituation(elaboration)
      case "я":
      case "Я":
        def noun = state.newFrame()
        def verb = state.newFrame()
        return state.assign(noun, 'type', 'ME').apply('nom', noun:noun, head:verb)
      case "мое":
        def noun = state.newFrame()
        def possHead = state.constructions.whatA ? state.constructions.whatA.head : state.constructions.possessive ? state.constructions.possessive.possHead : state.newFrame()
        state = state.assign(noun, 'type', 'ME')
        return state.apply('possessive', possessor:noun, head:possHead, conj:state.constructions.possessive)
      case "и": return state
      case "их":
        def noun = state.newFrame()
        def verb = state.constructions.acc ? state.constructions.acc.head : state.newFrame()
        def possHead = state.constructions.whatA ? state.constructions.whatA.head : state.newFrame()
        state = state.assign(noun, 'type', 'THEY').apply('acc', noun:noun, head:verb)
        return state.apply('possessive', possessor:noun, head:possHead)
      case "они":
        def nom = state.constructions.nom
        def noun = state.newFrame()
        def head = nom?.noun ? nom.head : state.newFrame()
        state = state.assign(noun, 'type', 'THEY')
        return state.apply('nom', noun:noun, head: head)
      case "соседям":
        def noun = state.constructions.dat?.noun ?: state.newFrame()
        return state.apply('dat', noun:noun) { it.assign(noun, 'type', 'NEIGHBOURS') }
      case "порядок":
        def acc = state.constructions.acc
        if (acc) {
          def noun = state.newFrame()
          state = state.assign(noun, 'type', 'ORDER')
          return state.apply('acc', noun:noun).apply('nounGen', head:noun)
        }
        return state
      case "счета":
        def nounGen = state.constructions.nounGen
        if (nounGen) {
          def noun = state.newFrame()
          state = state.assign(noun, 'type', 'COUNTING')
          return state.apply('nounGen', noun:noun)
        }
        return state
      case "вдруг":
        def nom = state.constructions.nom
        if (nom) {
          return state.assign(nom.head, 'manner', 'SUDDENLY')
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
          Variable verb = nom.head
          state = state.assign(verb, 'type', 'FORGET').assign(situation, 'time', 'PAST')
          return state.apply('comp', head:verb).apply('nom')
        }
        return state
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
          def verb = nom.head
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
          def verb = nom.head
          state = state.assign(verb, 'type', 'THINK').assign(situation, 'time', 'PRESENT')
          return state.apply('poDat', head:verb).apply('nom', head:verb).apply('acc')
        }
        return state
      case "спросил":
        def nom = state.constructions.nom
        if (nom) {
          def verb = state.newFrame()
          state = state.assign(verb, 'type', 'ASK')//todo don't reassign tense .assign(situation, 'time', 'PAST', false)
          return state.apply('acc', head:verb).apply('nom', head:verb).apply('comp', head:verb)
        }
        return state
      case ",":
        def next = new Situation()
        def comp = state.constructions.comp
        if (comp) {
          return state.apply('comp', comp:next).withSituation(next).apply(comp.head.frame(state.chart).type in ['DISCOVER', 'AMAZE'] ? 'declComp' : 'question', situation:next)
        }
        return state
      case "что":
        if (state.constructions.question) {
          def noun = state.newFrame()
          def verb = state.newFrame()
          return state.apply('nom', noun:noun, head:verb).apply('acc', noun:noun, head:verb).apply('question', questioned:noun)
        }
        return state
      case "идет":
        def nom = state.constructions.nom
        if (nom) {
          Variable verb = nom.head
          state = state.assign(situation, 'time', 'PRESENT')
          return state.apply('nom').apply('comeScalarly', verb:verb)
        }
        return state
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
      case "7":
      case "8":
        if (state.constructions.questionVariants) {
          return state.apply('questionVariants', variant:word)
        }
        return state
      case 'Каково':
        def degree = state.newFrame()
        def noun = state.newFrame()
        state = state.assign(situation, 'exclamation', degree)
        return state.assign(noun, 'degree', degree).apply('whatA', degree:degree, head:noun, situation:situation)
      case 'было':
        if (state.constructions.whatA) {
          return state.apply('whatA', time:'PAST')
        }
        return state.assign(situation, 'time', 'PAST')
    }
    return state
  }

  private ParsingState noun(ParsingState state, String caze, Closure init) {
    def noun = state.constructions[caze]?.noun ?: state.newFrame()
    return state.apply(caze, noun: noun) { init(it, noun) }
  }

  private ParsingState preposition(ParsingState state, String prepCtx, String caze) {
    state = state.apply(prepCtx)
    def save = state.constructions
    return state.clearConstructions().apply(caze, save: save, delegate: prepCtx)
  }

}