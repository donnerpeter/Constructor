package cons3

/**
 * @author peter
 */
class ParsingState {
  final Chart chart
  final Situation situation
  private final Variable lastFrame
  final Pair<String, Function1<ParsingState, ParsingState>> expectation
  private Map<String, Variable> participants = [:]

  ParsingState(Map map) {
    chart = map.chart
    situation = map.situation
    lastFrame = map.lastFrame
    expectation = map.expectation
    participants = map.participants
  }

  Frame getLastFrame() {
    return lastFrame?.frame(chart)
  }

  ParsingState clone(Map update) {
    Map current = [chart:chart, situation:situation, lastFrame:lastFrame, expectation:expectation, participants:participants]
    current.putAll(update)
    return new ParsingState(current)
  }

  ParsingState withFrame(Chart chart, Frame frame) {
    assert frame
    clone(chart:chart, lastFrame:frame.var, expectation:null)
  }

  ParsingState withSituation(Chart chart, Situation situation) { clone(chart:chart, situation:situation, lastFrame:null, expectation:null, participants:[:]) }

  ParsingState withExpectation(String expectation, Function1<ParsingState, ParsingState> r = {} as Function1) { clone(expectation:new Pair(expectation, r)) }

  ParsingState withRole(Chart chart, String key, Frame frame = null) {
    if (!frame) {
      frame = getAt(key)
      if (!frame) {
        (chart, frame) = newFrame()
      }
    }
    clone(chart:chart, lastFrame:frame.var, expectation:null, participants:(participants + [(key): frame.var]))
  }

  Frame getAt(String key) { participants[key]?.frame(chart) }

  Frame getDomain() { getAt('domain') }

  List newFrame() {
    def (ch, var) = chart.newFrame(situation)
    [ch, var.frame(ch)]
  }

  ParsingState withChart(Chart chart) { clone(chart:chart) }

  ParsingState assign(Frame frame, String property, value, boolean rheme) {
    withChart(chart.assign(frame.var, property, value instanceof Frame ? value.var : value, rheme))
  }
  ParsingState assign(Situation frame, String property, value, boolean rheme) {
    withChart(chart.assign(frame, property, value instanceof Frame ? value.var : value, rheme))
  }

}
