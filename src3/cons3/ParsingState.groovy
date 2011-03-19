package cons3

/**
 * @author peter
 */
class ParsingState {
  final Chart chart
  final Situation situation
  Map<String, Map> constructions = [:]

  ParsingState(Map map) {
    chart = map.chart
    situation = map.situation
    constructions = map.constructions
  }

  ParsingState clone(Map update) {
    Map current = [chart:chart, situation:situation, constructions:constructions]
    current.putAll(update)
    return new ParsingState(current)
  }

  ParsingState withSituation(Chart chart = this.chart, Situation situation) { clone(chart:chart, situation:situation, participants:[:], constructions:[:]) }

  List newFrame() {
    def (ch, var) = chart.newFrame(situation)
    [ch, var.frame(ch)]
  }

  ParsingState withChart(Chart chart) { clone(chart:chart) }

  ParsingState assign(Frame frame, String property, value, boolean rheme) {
    withChart(chart.assign(frame.var, property, value instanceof Frame ? value.var : value, rheme))
  }
  ParsingState assign(Variable var, String property, value, boolean rheme) {
    withChart(chart.assign(var, property, value instanceof Frame ? value.var : value, rheme))
  }


  private Chart _apply(Chart chart, String name, Map args) {
    switch (name) {
      case 'adjective':
        return chart.assign(args.nounFrame, args.rel, args.val, args.rheme)
      case 'nom':
        return args.head.frame(chart).type ? chart.assign(args.head, 'arg1', args.noun, args.rheme) : chart
      case 'acc':
        return args.head.frame(chart).type && args.noun ? chart.assign(args.head, 'arg2', args.noun, args.rheme) : chart
      case 'sInstr':
        return args.noun ? chart.assign(args.head, 'experiencer', args.noun, true) : chart
      case 'poDat':
        return args.noun ? chart.assign(args.head, 'topic', args.noun, false) : chart
      case 'nounGen':
        return args.noun ? chart.assign(args.head, 'criterion', args.noun, false) : chart
      case 'kDat':
        return args.noun ? chart.assign(args.head, 'goal', args.noun, true) : chart
      case 'comp':
        return args.comp ? chart.assign(args.head, args.head.frame(chart).type in ['FORGET', 'DISCOVER', 'AMAZE'] ? 'theme' : 'question', args.comp, args.rheme) : chart
      case 'question':
        return args.questioned ? chart.assign(args.situation, 'questioned', args.questioned, true) : chart
      case 'comeScalarly':
        return args.order ? chart.assign(args.verb, 'type', 'COME_SCALARLY', false).assign(args.verb, 'order', args.order, false) : chart
      case 'questionVariants':
        return args.variant ? chart.assign(args.questioned, 'variant', args.variant, false) : chart
      case 'whatA':
        return args.time ? chart.assign(args.situation, 'time', args.time, false) : chart
      case 'possessive':
        chart = args.conj ? _apply(chart, 'possessive', args.conj) : chart //todo generic conj handling
        return args.head.frame(chart).type ? chart.assign(args.head, 'arg1', args.possessor, true) : chart
      case 'control':
        return args.slave ? chart.assign(args.head, 'theme', args.slave, false) : chart
    }

    return chart
  }

  ParsingState apply(Map newArgs = [:], String name) {
    apply(newArgs, chart, name)
  }

  ParsingState apply(Map newArgs = [:], Chart chart, String name) {
    def args = constructions.get(name, [:]) + newArgs
    def newConstructions = constructions + [(name): args]
    return clone(constructions: newConstructions).withChart(_apply(chart, name, args))
  }


}