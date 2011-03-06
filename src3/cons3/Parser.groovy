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
        frame = getAt(key) ?: situation.newFrame()
      }
      clone(chart:chart, lastFrame:frame, expectation:null, participants:(participants + [(key): frame]))
    }

    Frame getAt(String key) { participants[key] }

    Frame getDomain() { getAt('domain') }

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
        situation.assign(situation, 'elaboration', elaboration, true)
        return state.withSituation(state.chart, elaboration)
      case "я":
      case "Я": return noun(state, 'nom', 'ME', false)
      case "мое":
        def me = situation.newFrame()
        situation.assign(me, 'type', 'ME', false)
        def prev = state['acc']
        if (prev) {
          situation.assign(state['nom'], 'arg1', prev, true) //todo acc/gen ambiguity
        }
        situation.assign(state['nom'], 'arg1', me, true)
        return state
      case "и": return state.withExpectation(null)
      case "их":
        state = noun(state, 'acc', 'THEY', false)
        return state.withExpectation('noun', { st ->
          def they = situation.newFrame()
          situation.assign(they, 'type', 'THEY', false)
          situation.assign(state['nom'], 'arg1', they, true) //todo possessive for differently cased NPs
          st
        } as Function1)
      case "они": return noun(state, 'nom', 'THEY', false)
      case "соседям": return noun(state, 'dat', 'NEIGHBOURS', false)
      case "порядок": return noun(state, 'acc', 'ORDER', false)
      case "счета": return noun(state, 'gen', 'COUNTING', false)
      case "вдруг":
        state = state.withRole(state.chart, 'domain')
        situation.assign(state.domain, "manner", "SUDDENLY", true)
        return state
      case "тоже":
        def also = situation.newFrame()
        def subj = situation.newFrame()
        situation.assign(also, 'type', 'ALSO', true)
        situation.assign(also, 'arg1', subj, true)
        state = state.withRole(state.chart, 'nom', subj).withRole(state.chart, 'domain')
        situation.assign(also, 'theme', state.domain, true)
        return state
      case "не":
        situation.assign(state.domain, "negated", "true", false)
        return state
      case "забыл": return verb(state, 'FORGET', 'PAST', true)
      case "могут":
        def subj = state['nom']
        state = verb(state, 'CAN', 'PRESENT', false)
        situation.assign(subj, 'type', 'THEY', false)
        return state
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
          situation.assign(domain, role, next, state.domain != null)
        }
        return state.withSituation(state.chart, next).withExpectation(domain?.type == 'DISCOVER' ? 'fact' : 'question')
      case "что":
        if (state.expectation?.first == 'fact') {
          return state.withExpectation(null)
        }
        state = state.withRole(state.chart, 'nom')
        situation.assign(situation, "questioned", state.lastFrame, true)
        return state.withRole(state.chart, 'questioned', state.lastFrame)
      case "идет": return verb(state, 'COME_SCALARLY', 'PRESENT', false)
      case "раньше":
        situation.assign(state.domain, 'order', 'EARLIER', false)
        return state
      case "-":
        return state.withFrame(state.chart, state['nom'])
      case "7":
      case "8":
        situation.assign(state.lastFrame, 'variant', word, false)
        return state
      case 'Каково':
        def degree = situation.newFrame()
        situation.assign(situation, 'exclamation', degree , true)
        state = state.withRole(state.chart, 'nom')
        situation.assign(state.lastFrame, 'degree', degree, true)
        return state
      case 'было':
        situation.assign(situation, 'time', 'PAST' , false)
        return state
    }
    return state
  }

  private ParsingState verb(ParsingState state, String type, String time, boolean rheme) {
    def situation = state.situation
    state = state.withRole(state.chart, 'domain', !state.domain || state.domain.type ? situation.newFrame() : state.domain)
    situation.assign(state.domain, "type", type, rheme)
    if (!situation.s('time')) {
      situation.assign(situation, "time", time, false)
    }
    thetas(state)
    return state
  }

  private ParsingState infinitive(ParsingState state, String type, boolean rheme) {
    def situation = state.situation
    def oldDomain = state.domain
    def control = oldDomain?.type == 'CAN'
    state = state.withRole(state.chart, 'domain', control || !oldDomain || oldDomain.type ? situation.newFrame() : oldDomain)
    if (control) {
      situation.assign(oldDomain, 'theme', state.domain, rheme)
    }
    situation.assign(state.domain, "type", type, rheme)
    thetas(state)
    return state
  }

  private ParsingState noun(ParsingState state, String caze, String type, boolean rheme) {
    def oldLast = state.lastFrame
    def expectation = state.expectation

    def existing = state[caze]
    if (existing && (existing.s('type') || existing == state['questioned'])) {
      state = state.withRole(state.chart, 'acc', existing).withRole(state.chart, caze, state.situation.newFrame())
    } else {
      state = state.withRole(state.chart, caze)
    }

    if (expectation?.first == 'noun') {
      state = expectation.second.call(state)
    }

    state.situation.assign(state.lastFrame, "type", type, rheme)

    if (caze == 'gen' && oldLast?.type == 'ORDER') {
      state.situation.assign(oldLast, 'criterion', state.lastFrame, rheme)
    }

    thetas(state)
    return state
  }

  private ParsingState adj(ParsingState state, String attr, String value, boolean rheme, String caze) {
    state = state.withRole(state.chart, caze)
    state.situation.assign(state.lastFrame, attr, value, rheme)
    thetas(state)
    return state
  }

  private void thetas(ParsingState state) {
    def verb = state.domain
    def type = verb?.type
    if (!type) return


    if (type == 'THINK') {
      ensureTheta state, 'nom', 'arg1', false
      ensureTheta state, 'acc', 'opinion', false
      ensureTheta state, 'dat', 'topic', false //todo poDat
    }
    if (type == 'HAPPEN') {
      ensureTheta state, 'nom', 'arg1', true
      ensureTheta state, 'instr', 'experiencer', true //todo sInstr
    }
    if (type == 'COME_SCALARLY') {
      ensureTheta state, 'nom', 'arg1', false
    }
    if (type == 'GO_OFF') {
      ensureTheta state, 'nom', 'arg1', true
      ensureTheta state, 'dat', 'goal', true //todo kDat
    }
    if (type == 'ASK') {
      ensureTheta state, 'nom', 'arg1', true
      ensureTheta state, 'acc', 'arg2', true
    }
    if (type == 'FORGET') {
      ensureTheta state, 'nom', 'arg1', true
    }
    if (type == 'DISCOVER') {
      ensureTheta state, 'nom', 'arg1', true
    }
    if (type == 'REMEMBER') {
      ensureTheta state, 'acc', 'arg2', false
    }

  }

  private void ensureTheta(ParsingState state, String caze, String theta, boolean rheme) {
    if (state[caze] && !state.domain.f(theta)) {
      state.situation.assign(state.domain, theta, state[caze], rheme)
    }
  }

}