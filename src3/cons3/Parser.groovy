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
        return state.apply(ch1, 'adjective', nounFrame:noun.var, rel:'property', val:'AMAZING').apply('nom', noun:noun.var, head:verb)
      case "этому": return adj(state, 'determiner', 'THIS', false, 'dat')
      case "случай":
        def adj = state.constructions.adjective
        if (adj) {
          Variable noun = adj.nounFrame
          return state.withChart(state.chart.assign(noun, 'type', 'THING', true))
        }
        return noun(state, 'nom', "THING", true)
      case "удивление": return noun(state, 'nom', "AMAZE", true)
      case "поводу": return noun(state, 'dat', "MATTER", false)
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
        def (ch, me) = state.newFrame()
        ch = ch.assign(me.var, 'type', 'ME', false)
        def prev = state['acc']
        if (prev) {
          ch = ch.assign(state['nom'].var, 'arg1', prev.var, true) //todo acc/gen ambiguity
        }
        ch = ch.assign(state['nom'].var, 'arg1', me.var, true)
        return state.withChart(ch)
      case "и": return state.withExpectation(null)
      case "их":
        if (state.constructions.acc) {
          def (ch, noun) = state.newFrame()
          return state.assign(noun, 'type', 'THEY', false).apply('acc', noun:noun.var)
        }

        state = noun(state, 'acc', 'THEY', false)
        return state.withExpectation('noun', { st ->
          def (ch, they) = state.newFrame()
          ch = ch.assign(they.var, 'type', 'THEY', false)
          ch = ch.assign(state['nom'].var, 'arg1', they.var, true) //todo possessive for differently cased NPs
          st.withChart(ch)
        } as Function1)
      case "они": return noun(state, 'nom', 'THEY', false)
      case "соседям":
        def kDat = state.constructions.kDat
        if (kDat) {
          def (ch, noun) = state.newFrame()
          ch = ch.assign(noun.var, 'type', 'NEIGHBOURS', false)
          return state.apply(ch, 'kDat', noun:noun.var)
        }
        return noun(state, 'dat', 'NEIGHBOURS', false)
      case "порядок": return noun(state, 'acc', 'ORDER', false)
      case "счета": return noun(state, 'gen', 'COUNTING', false)
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
        state = state.withRole(ch1, 'nom', subj.frame(ch1)).withRole(ch1, 'domain')
        return state.assign(also, 'theme', state.domain, true)
      case "не":
        return state.assign(state.domain, "negated", "true", false)
      case "забыл":
        def nom = state.constructions.nom
        if (nom) {
          Variable verb = nom.head
          def ch = state.chart.assign(verb, 'type', 'FORGET', true).assign(situation, 'time', 'PAST', false)
          return state.apply('comp', head:verb).apply(ch, 'nom', rheme:true)
        }
        return verb(state, 'FORGET', 'PAST', true)
      case "могут":
        def subj = state['nom']
        state = verb(state, 'CAN', 'PRESENT', false)
        return state.assign(subj, 'type', 'THEY', false)
      case "отправился":
        def nom = state.constructions.nom
        if (nom) {
          def verb = nom.head
          def ch = state.chart.assign(verb, 'type', 'GO_OFF', true).assign(situation, 'time', 'PAST', false)
          return state.apply('kDat', head:verb).apply(ch, 'nom', rheme:true, head:verb)
        }
        return verb(state, 'GO_OFF', 'PAST', true)
      case "обнаружили": return verb(state, 'DISCOVER', 'PAST', true)
      case "вспомнить": return infinitive(state, 'REMEMBER', false)
      case "думают": return verb(state, 'THINK', 'PRESENT', false)
      case "спросил":
        def nom = state.constructions.nom
        if (nom) {
          def (ch, verb) = state.newFrame()
          ch = ch.assign(verb.var, 'type', 'ASK', true)//todo don't reassign tense .assign(situation, 'time', 'PAST', false)
          return state.apply('acc', head:verb.var, rheme:true).apply(ch, 'nom', rheme:true, head:verb.var).apply('comp', head:verb.var)
        }
        return verb(state, 'ASK', 'PAST', true)
      case ",":
        def (ch, next) = state.chart.newSituation()
        state = state.withChart(ch)
        if (state.constructions.comp) {
          return state.apply('comp', comp:next).withSituation(next).withExpectation('question')
        }

        def domain = state.domain ?: state.lastFrame //todo late closure
        if (domain) {
          def role = domain.type in ['FORGET', 'AMAZE', 'DISCOVER'] ? 'theme' : 'question'
          state = state.assign(domain, role, next, state.domain != null)
        }
        return state.withSituation(next).withExpectation(domain?.type == 'DISCOVER' ? 'fact' : 'question')
      case "что":
        if (state.expectation?.first == 'fact') {
          return state.withExpectation(null)
        }
        def (ch, noun) = state.newFrame()
        def (ch1, verb) = ch.newFrame(state.situation)
        return state.apply(ch1, 'nom', noun:noun.var, head:verb, rheme:false).apply('question', questioned:noun.var, situation:situation)
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
        return state.assign(state.lastFrame, 'degree', degree, true)
      case 'было':
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