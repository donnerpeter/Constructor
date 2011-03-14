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
  Map<String, Map> constructions = [:]

  ParsingState(Map map) {
    chart = map.chart
    situation = map.situation
    lastFrame = map.lastFrame
    expectation = map.expectation
    participants = map.participants
    constructions = map.constructions
  }

  Frame getLastFrame() {
    return lastFrame?.frame(chart)
  }

  ParsingState clone(Map update) {
    Map current = [chart:chart, situation:situation, lastFrame:lastFrame, expectation:expectation, participants:participants, constructions:constructions]
    current.putAll(update)
    return new ParsingState(current)
  }

  ParsingState withFrame(Chart chart, Frame frame) {
    assert frame
    clone(chart:chart, lastFrame:frame.var, expectation:null)
  }

  ParsingState withSituation(Chart chart = this.chart, Situation situation) { clone(chart:chart, situation:situation, lastFrame:null, expectation:null, participants:[:], constructions:[:]) }

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

  ParsingState withConstruction(Map args, String cxt) {
    clone(constructions:(constructions + [(cxt):args]))
  }

  private Chart _apply(Chart chart, String name, Map args) {
    switch (name) {
      case 'adjective':
        return chart.assign(args.nounFrame, args.rel, args.val, true)
      case 'nom':
        return args.head.frame(chart).type ? chart.assign(args.head, 'arg1', args.noun, args.rheme) : chart
      case 'acc':
        return args.head.frame(chart).type ? chart.assign(args.head, 'arg2', args.noun, args.rheme) : chart
      case 'sInstr':
        return args.noun ? chart.assign(args.head, 'experiencer', args.noun, true) : chart
      case 'kDat':
        return args.noun ? chart.assign(args.head, 'goal', args.noun, true) : chart
      case 'comp':
        return args.comp ? chart.assign(args.head, args.head.frame(chart).type == 'FORGET' ? 'theme' : 'question', args.comp, true) : chart
      case 'question':
        return chart.assign(args.situation, 'questioned', args.questioned, true)
      case 'comeScalarly':
        return args.order ? chart.assign(args.verb, 'type', 'COME_SCALARLY', false).assign(args.verb, 'order', args.order, false) : chart
      case 'questionVariants':
        return args.variant ? chart.assign(args.questioned, 'variant', args.variant, false) : chart
    }

    return chart
  }

  ParsingState apply(Map newArgs = [:], String name) {
    apply(newArgs, chart, name)
  }

  ParsingState apply(Map newArgs = [:], Chart chart, String name) {
    def args = constructions.get(name, [:]) + newArgs
    return withConstruction(args, name).withChart(_apply(chart, name, args))
  }


}
