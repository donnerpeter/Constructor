package cons3

/**
 * @author peter
 */
class Parser {

  Chart parse(String text) {
    def chart = new Chart()
    ParsingState state = new ParsingState(chart:chart, situation:chart.newSituation(), participants:[:])
    def tokenizer = new StringTokenizer(text, """ '":,.""", true)
    for (String w in tokenizer) {
      if (w != ' ') {
        state = handleWord(w, state)
      }
    }
    return state.chart
  }

  static class ParsingState {
    final Chart chart
    final Situation situation
    final Frame lastFrame
    final Pair<String, Function1<ParsingState, ParsingState>> expectation
    private Map<String, Frame> participants = [:]

    ParsingState(Map map) {
      chart = map.chart
      situation = map.situation
      lastFrame = map.lastFrame
      expectation = map.expectation
      participants = map.participants
    }

    ParsingState clone(Map update) {
      Map current = [chart:chart, situation:situation, lastFrame:lastFrame, expectation:expectation, participants:participants]
      current.putAll(update)
      return new ParsingState(current)
    }

    ParsingState withFrame(Chart chart, Frame frame) {
      assert frame
      clone(chart:chart, lastFrame:frame, expectation:null)
    }

    ParsingState withSituation(Chart chart, Situation situation) { clone(chart:chart, situation:situation, lastFrame:null, expectation:null, participants:[:]) }

    ParsingState withExpectation(String expectation, Function1<ParsingState, ParsingState> r = {} as Function1) { clone(expectation:new Pair(expectation, r)) }

    ParsingState withRole(Chart chart, String key, Frame frame = null) {
      if (!frame) {
        frame = getAt(key)
        if (!frame) {
          (chart, frame) = chart.newFrame(situation)
        }
      }
      clone(chart:chart, lastFrame:frame, expectation:null, participants:(participants + [(key): frame]))
    }

    Frame getAt(String key) { participants[key] }

    Frame getDomain() { getAt('domain') }

    List newFrame() {
      chart.newFrame(situation)
    }

    ParsingState withChart(Chart chart) { clone(chart:chart) }

    ParsingState assign(Frame frame, String property, String value, boolean rheme) {
      withChart(chart.assign(frame, property, value, rheme))
    }

    ParsingState assign(Frame frame, String property, Frame value, boolean rheme) {
      withChart(chart.assign(frame, property, value, rheme))
    }

  }

  ParsingState handleWord(String word, ParsingState state) {
    def situation = state.situation
    switch (word) {
      case "Удивительный": return adj(state, "property", "AMAZING", true, 'nom')
      case "этому": return adj(state, 'determiner', 'THIS', false, 'dat')
      case "случай": return noun(state, 'nom', "THING", true)
      case "удивление": return noun(state, 'nom', "AMAZE", true)
      case "поводу": return noun(state, 'dat', "MATTER", false)
      case "случился": return verb(state, "HAPPEN", "PAST", true)
      case "мной": return noun(state, 'instr', 'ME', false)
      case ":":
        def elaboration = situation.chart.newSituation()
        state = state.assign(situation, 'elaboration', elaboration, true)
        return state.withSituation(state.chart, elaboration)
      case "я":
      case "Я": return noun(state, 'nom', 'ME', false)
      case "мое":
        def (ch, me) = state.newFrame()
        ch = ch.assign(me, 'type', 'ME', false)
        def prev = state['acc']
        if (prev) {
          ch = ch.assign(state['nom'], 'arg1', prev, true) //todo acc/gen ambiguity
        }
        ch = ch.assign(state['nom'], 'arg1', me, true)
        return state.withChart(ch)
      case "и": return state.withExpectation(null)
      case "их":
        state = noun(state, 'acc', 'THEY', false)
        return state.withExpectation('noun', { st ->
          def (ch, they) = state.newFrame()
          ch = ch.assign(they, 'type', 'THEY', false)
          ch = ch.assign(state['nom'], 'arg1', they, true) //todo possessive for differently cased NPs
          st.withChart(ch)
        } as Function1)
      case "они": return noun(state, 'nom', 'THEY', false)
      case "соседям": return noun(state, 'dat', 'NEIGHBOURS', false)
      case "порядок": return noun(state, 'acc', 'ORDER', false)
      case "счета": return noun(state, 'gen', 'COUNTING', false)
      case "вдруг":
        state = state.withRole(state.chart, 'domain')
        def ch = state.chart.assign(state.domain, "manner", "SUDDENLY", true)
        return state.withChart(ch)
      case "тоже":
        def (ch, also) = state.newFrame()
        def (ch1, subj) = ch.newFrame(situation)
        ch1 = ch1.assign(also, 'type', 'ALSO', true)
        ch1 = ch1.assign(also, 'arg1', subj, true)
        state = state.withRole(ch1, 'nom', subj).withRole(ch1, 'domain')
        return state.assign(also, 'theme', state.domain, true)
      case "не":
        return state.assign(state.domain, "negated", "true", false)
      case "забыл": return verb(state, 'FORGET', 'PAST', true)
      case "могут":
        def subj = state['nom']
        state = verb(state, 'CAN', 'PRESENT', false)
        return state.assign(subj, 'type', 'THEY', false)
      case "отправился": return verb(state, 'GO_OFF', 'PAST', true)
      case "обнаружили": return verb(state, 'DISCOVER', 'PAST', true)
      case "вспомнить": return infinitive(state, 'REMEMBER', false)
      case "думают": return verb(state, 'THINK', 'PRESENT', false)
      case "спросил": return verb(state, 'ASK', 'PAST', true)
      case ",":
        def next = situation.chart.newSituation()
        def domain = state.domain ?: state.lastFrame //todo late closure
        if (domain) {
          def role = domain.type in ['FORGET', 'AMAZE', 'DISCOVER'] ? 'theme' : 'question'
          state = state.assign(domain, role, next, state.domain != null)
        }
        return state.withSituation(state.chart, next).withExpectation(domain?.type == 'DISCOVER' ? 'fact' : 'question')
      case "что":
        if (state.expectation?.first == 'fact') {
          return state.withExpectation(null)
        }
        state = state.withRole(state.chart, 'nom')
        state = state.assign(situation, "questioned", state.lastFrame, true)
        return state.withRole(state.chart, 'questioned', state.lastFrame)
      case "идет": return verb(state, 'COME_SCALARLY', 'PRESENT', false)
      case "раньше":
        return state.assign(state.domain, 'order', 'EARLIER', false)
      case "-":
        return state.withFrame(state.chart, state['nom'])
      case "7":
      case "8":
        return state.assign(state.lastFrame, 'variant', word, false)
      case 'Каково':
        def (ch, degree) = state.newFrame()
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
    state = state.withRole(state.chart, 'domain', domain)
    state = state.assign(state.domain, "type", type, rheme)
    if (!situation.s('time')) {
      state = state.assign(situation, "time", time, false)
    }
    return thetas(state)
  }

  private ParsingState infinitive(ParsingState state, String type, boolean rheme) {
    def situation = state.situation
    def oldDomain = state.domain
    def control = oldDomain?.type == 'CAN'
    def (ch, domain) = control || !oldDomain || oldDomain.type ? state.newFrame() : [state.chart, oldDomain]
    state = state.withRole(state.chart, 'domain', domain)
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