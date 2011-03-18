package cons3

/**
 * @author peter
 */
class Parser {

  Chart parse(String text) {
    def (chart, situation) = new Chart().newSituation()
    ParsingState state = new ParsingState(chart:chart, situation:situation, participants:[:], constructions:[:])
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
        def (ch, noun) = state.newFrame()
        def (ch1, verb) = ch.newFrame(state.situation)
        return state.apply(ch1, 'adjective', nounFrame:noun.var, rel:'property', val:'AMAZING', rheme:true).apply('nom', noun:noun.var, head:verb)
      case "этому":
        if (state.constructions.poDat) {
          def (ch, noun) = state.newFrame()
          return state.apply(ch, 'adjective', nounFrame:noun.var, rel:'determiner', val:'THIS', rheme:false).apply('poDat', noun:noun.var)
        }
        return state
      case "случай":
        def adj = state.constructions.adjective
        if (adj) {
          Variable noun = adj.nounFrame
          return state.withChart(state.chart.assign(noun, 'type', 'THING', true))
        }
        return noun(state, 'nom', "THING", true)
      case "удивление":
        def poss = state.constructions.possessive
        if (poss) {
          return state.assign(poss.head.frame(state.chart), 'type', 'AMAZE', true).apply('possessive').apply('comp', head:poss.head, rheme:false)
        }
        return state
      case "поводу":
        def adj = state.constructions.adjective
        if (adj) {
          Variable noun = adj.nounFrame
          return state.withChart(state.chart.assign(noun, 'type', 'MATTER', false))
        }
        return state
      case "случился":
        def nom = state.constructions.nom
        if (nom) {
          Variable verb = nom.head
          def ch = state.chart.assign(verb, 'type', 'HAPPEN', true).assign(situation, 'time', 'PAST', false)
          return state.apply('sInstr', head:verb).apply(ch, 'nom', rheme:true)
        }
        return verb(state, "HAPPEN", "PAST", true)
      case "мной":
        def sInstr = state.constructions.sInstr
        if (sInstr) {
          def (ch, noun) = state.newFrame()
          ch = ch.assign(noun.var, 'type', 'ME', false)
          return state.apply(ch, 'sInstr', noun:noun.var)
        }
        return noun(state, 'instr', 'ME', false)
      case ":":
        def (ch, elaboration) = state.chart.newSituation()
        state = state.withChart(ch).assign(situation, 'elaboration', elaboration, true)
        return state.withSituation(elaboration)
      case "я":
      case "Я":
        def (ch, noun) = state.newFrame()
        def (ch1, verb) = ch.newFrame(situation)
        return state.withChart(ch1).assign(noun, 'type', 'ME', false).apply('nom', noun:noun.var, head:verb)
      case "мое":
        def (ch, noun) = state.newFrame()
        def (ch2, possHead) = state.constructions.whatA ? [ch, state.constructions.whatA.head] : state.constructions.possessive ? [ch, state.constructions.possessive.possHead] : ch.newFrame(state.situation)
        state = state.withChart(ch2).assign(noun, 'type', 'ME', false)
        return state.apply('possessive', possessor:noun.var, head:possHead, conj:state.constructions.possessive)
      case "и": return state.withExpectation(null)
      case "их":
        def (ch, noun) = state.newFrame()
        def (ch1, verb) = state.constructions.acc ? [ch, state.constructions.acc.head] : ch.newFrame(state.situation)
        def (ch2, possHead) = state.constructions.whatA ? [ch1, state.constructions.whatA.head] : ch1.newFrame(state.situation)
        state = state.withChart(ch2).assign(noun, 'type', 'THEY', false).apply('acc', noun:noun.var, head:verb)
        return state.apply('possessive', possessor:noun.var, head:possHead)
      case "они":
        def nom = state.constructions.nom
        def (ch, noun) = state.newFrame()
        def (ch1, head) = nom?.noun ? [ch, nom.head] : ch.newFrame(state.situation)
        ch1 = ch1.assign(noun.var, 'type', 'THEY', false)
        return state.withChart(ch1).apply('nom', noun:noun.var, head: head)
      case "соседям":
        def kDat = state.constructions.kDat
        if (kDat) {
          def (ch, noun) = state.newFrame()
          ch = ch.assign(noun.var, 'type', 'NEIGHBOURS', false)
          return state.apply(ch, 'kDat', noun:noun.var)
        }
        return noun(state, 'dat', 'NEIGHBOURS', false)
      case "порядок":
        def acc = state.constructions.acc
        if (acc) {
          def (ch, noun) = state.newFrame()
          ch = ch.assign(noun.var, 'type', 'ORDER', false)
          return state.apply(ch, 'acc', noun:noun.var, rheme:false).apply('nounGen', head:noun.var)
        }
        return state
      case "счета":
        def nounGen = state.constructions.nounGen
        if (nounGen) {
          def (ch, noun) = state.newFrame()
          ch = ch.assign(noun.var, 'type', 'COUNTING', false)
          return state.apply(ch, 'nounGen', noun:noun.var)
        }
        return state
      case "вдруг":
        def nom = state.constructions.nom
        if (nom) {
          return state.assign(nom.head.frame(state.chart), 'manner', 'SUDDENLY', true)
        }
        state = state.withRole(state.chart, 'domain')
        def ch = state.chart.assign(state.domain.var, "manner", "SUDDENLY", true)
        return state.withChart(ch)
      case "тоже":
        def (ch, also) = state.newFrame()
        def (ch1, subj) = ch.newFrame(situation)
        ch1 = ch1.assign(also.var, 'type', 'ALSO', true)
        ch1 = ch1.assign(also.var, 'arg1', subj, true)
        return state.withChart(ch1).apply(ch1, 'also', also:also.var, subj:subj)
      case "не":
        return state.apply('negation')
      case "забыл":
        def nom = state.constructions.nom
        if (nom) {
          Variable verb = nom.head
          def ch = state.chart.assign(verb, 'type', 'FORGET', true).assign(situation, 'time', 'PAST', false)
          return state.apply('comp', head:verb, rheme:true).apply(ch, 'nom', rheme:true)
        }
        return verb(state, 'FORGET', 'PAST', true)
      case "могут":
        def (ch, verb) = state.newFrame()
        state = state.withChart(ch)
        def also = state.constructions.also
        def subj
        if (also) {
          subj = also.subj
          state = state.assign(also.also, 'theme', verb.var, true)
        }
        if (state.constructions.negation != null) {
          state = state.assign(verb.var, 'negated', 'true', false)
        }
        if (!subj) {
          (ch, subj) = state.newFrame()
          state = state.withChart(ch)
        }
        state = state.assign(verb, 'type', 'CAN', false).assign(situation, 'time', 'PRESENT', false)
        state = state.assign(subj, 'type', 'THEY', false)
        return state.apply('control', subj:subj, head:verb.var)
      case "отправился":
        def nom = state.constructions.nom
        if (nom) {
          def verb = nom.head
          def ch = state.chart.assign(verb, 'type', 'GO_OFF', true).assign(situation, 'time', 'PAST', false)
          return state.apply('kDat', head:verb).apply(ch, 'nom', rheme:true, head:verb)
        }
        return verb(state, 'GO_OFF', 'PAST', true)
      case "обнаружили":
        def nom = state.constructions.nom
        if (nom) {
          def verb = nom.head
          def ch = state.chart.assign(verb, 'type', 'DISCOVER', true).assign(situation, 'time', 'PAST', false)
          return state.apply('comp', head:verb, rheme:true).apply(ch, 'nom', rheme:true, head:verb)
        }
        return verb(state, 'DISCOVER', 'PAST', true)
      case "вспомнить":
        def (ch, verb) = state.newFrame()
        state = state.withChart(ch).apply('control', slave:verb.var)
        return state.assign(verb, 'type', 'REMEMBER', false).apply('acc', head:verb.var)
      case "думают":
        def nom = state.constructions.nom
        if (nom) {
          def verb = nom.head
          def ch = state.chart.assign(verb, 'type', 'THINK', false).assign(situation, 'time', 'PRESENT', false)
          return state.apply('poDat', head:verb).apply(ch, 'nom', rheme:false, head:verb).apply('acc', rheme:false)
        }
        return verb(state, 'THINK', 'PRESENT', false)
      case "спросил":
        def nom = state.constructions.nom
        if (nom) {
          def (ch, verb) = state.newFrame()
          ch = ch.assign(verb.var, 'type', 'ASK', true)//todo don't reassign tense .assign(situation, 'time', 'PAST', false)
          return state.apply('acc', head:verb.var, rheme:true).apply(ch, 'nom', rheme:true, head:verb.var).apply('comp', head:verb.var, rheme:true)
        }
        return verb(state, 'ASK', 'PAST', true)
      case ",":
        def (ch, next) = state.chart.newSituation()
        state = state.withChart(ch)
        def comp = state.constructions.comp
        if (comp) {
          return state.apply('comp', comp:next).withSituation(next).apply(comp.head.frame(ch).type in ['DISCOVER', 'AMAZE'] ? 'declComp' : 'question', situation:next)
        }
        return state
      case "что":
        if (state.constructions.question) {
          def (ch, noun) = state.newFrame()
          def (ch1, verb) = ch.newFrame(state.situation)
          return state.apply(ch1, 'nom', noun:noun.var, head:verb, rheme:false).apply('acc', noun:noun.var, head:verb, rheme:false).apply('question', questioned:noun.var)
        }
        return state
      case "идет":
        def nom = state.constructions.nom
        if (nom) {
          Variable verb = nom.head
          def ch = state.chart.assign(situation, 'time', 'PRESENT', false)
          return state.apply(ch, 'nom').apply('comeScalarly', verb:verb)
        }
        return verb(state, 'COME_SCALARLY', 'PRESENT', false)
      case "раньше":
        def cs = state.constructions.comeScalarly
        if (cs) {
          return state.apply('comeScalarly', order:'EARLIER').apply('nom')
        }
        return state.assign(state.domain, 'order', 'EARLIER', false)
      case "-":
        if (state.constructions.question) {
          return state.apply('questionVariants', questioned:state.constructions.question.questioned)
        }
        return state.withFrame(state.chart, state['nom'])
      case "7":
      case "8":
        if (state.constructions.questionVariants) {
          return state.apply('questionVariants', variant:word)
        }
        return state.assign(state.lastFrame, 'variant', word, false)
      case 'Каково':
        def (ch, degree) = state.newFrame()
        state = state.withChart(ch)
        state = state.assign(situation, 'exclamation', degree, true)
        state = state.withRole(state.chart, 'nom')
        return state.assign(state.lastFrame, 'degree', degree, true).apply('whatA', degree:degree, head:state.lastFrame.var, situation:situation)
      case 'было':
        if (state.constructions.whatA) {
          return state.apply('whatA', time:'PAST')
        }
        return state.assign(situation, 'time', 'PAST' , false)
    }
    return state
  }

  private ParsingState verb(ParsingState state, String type, String time, boolean rheme) {
    def situation = state.situation
    def (ch, domain) = !state.domain || state.domain.type ? state.newFrame() : [state.chart, state.domain]
    state = state.withRole(ch, 'domain', domain)
    state = state.assign(state.domain, "type", type, rheme)
    if (!situation.frame(state.chart).s('time')) {
      state = state.assign(situation, "time", time, false)
    }
    return thetas(state)
  }

  private ParsingState infinitive(ParsingState state, String type, boolean rheme) {
    def oldDomain = state.domain
    def control = oldDomain?.type == 'CAN'
    def (ch, domain) = control || !oldDomain || oldDomain.type ? state.newFrame() : [state.chart, oldDomain]
    state = state.withRole(ch, 'domain', domain)
    if (control) {
      state = state.assign(oldDomain, 'theme', state.domain, rheme)
    }
    state = state.assign(state.domain, "type", type, rheme)
    return thetas(state)
  }

  private ParsingState noun(ParsingState state, String caze, String type, boolean rheme) {
    def oldLast = state.lastFrame
    def expectation = state.expectation

    def existing = state[caze]
    if (existing && (existing.s('type') || existing == state['questioned'])) {
      def (ch, f) = state.newFrame()
      state = state.withRole(ch, 'acc', existing).withRole(ch, caze, f)
    } else {
      state = state.withRole(state.chart, caze)
    }

    if (expectation?.first == 'noun') {
      state = expectation.second.call(state)
    }

    state = state.assign(state.lastFrame, "type", type, rheme)

    if (caze == 'gen' && oldLast?.type == 'ORDER') {
      state = state.assign(oldLast, 'criterion', state.lastFrame, rheme)
    }

    return thetas(state)
  }

  private ParsingState adj(ParsingState state, String attr, String value, boolean rheme, String caze) {
    state = state.withRole(state.chart, caze)
    state = state.assign(state.lastFrame, attr, value, rheme)
    return thetas(state)
  }

  private ParsingState thetas(ParsingState state) {
    def verb = state.domain
    def type = verb?.type
    if (!type) return state


    if (type == 'THINK') {
      state = ensureTheta(state, 'nom', 'arg1', false)
      state = ensureTheta(state, 'acc', 'opinion', false)
      state = ensureTheta(state, 'dat', 'topic', false) //todo poDat
    }
    if (type == 'HAPPEN') {
      state = ensureTheta(state, 'nom', 'arg1', true)
      state = ensureTheta(state, 'instr', 'experiencer', true) //todo sInstr
    }
    if (type == 'COME_SCALARLY') {
      state = ensureTheta(state, 'nom', 'arg1', false)
    }
    if (type == 'GO_OFF') {
      state = ensureTheta(state, 'nom', 'arg1', true)
      state = ensureTheta(state, 'dat', 'goal', true) //todo kDat
    }
    if (type == 'ASK') {
      state = ensureTheta(state, 'nom', 'arg1', true)
      state = ensureTheta(state, 'acc', 'arg2', true)
    }
    if (type == 'FORGET') {
      state = ensureTheta(state, 'nom', 'arg1', true)
    }
    if (type == 'DISCOVER') {
      state = ensureTheta(state, 'nom', 'arg1', true)
    }
    if (type == 'REMEMBER') {
      state = ensureTheta(state, 'acc', 'arg2', false)
    }
    return state
  }

  private ParsingState ensureTheta(ParsingState state, String caze, String theta, boolean rheme) {
    if (state[caze] && !state.domain.f(theta)) {
      state = state.assign(state.domain, theta, state[caze], rheme)
    }
    return state
  }

}