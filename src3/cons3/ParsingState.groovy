package cons3

/**
 * @author peter
 */
class ParsingState {
  static final Closure noInit = { it }

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

  ParsingState withSituation(Situation situation) { clone(situation:situation, constructions:[:]) }

  Variable newFrame() {
    new Variable(situation)
  }

  ParsingState withChart(Chart chart) { clone(chart:chart) }

  ParsingState assign(Variable var, String property, def value) {
    withChart(chart.assign(var, property, value))
  }


  private Chart _apply(Chart chart, String name, Map args) {
    switch (name) {
      case 'adjective':
        return chart.assign(args.nounFrame, args.rel, args.val)
      case 'nom':
        return args.head.frame(chart).type ? chart.assign(args.head, 'arg1', args.noun) : chart
      case 'acc':
        return args.head.frame(chart).type && args.noun ? chart.assign(args.head, 'arg2', args.noun) : chart
      case 'sInstr':
        return args.noun ? chart.assign(args.head, 'experiencer', args.noun) : chart
      case 'poDat':
        return args.noun ? chart.assign(args.head, 'topic', args.noun) : chart
      case 'nounGen':
        return args.noun ? chart.assign(args.head, 'criterion', args.noun) : chart
      case 'kDat':
        return args.noun ? chart.assign(args.head, 'goal', args.noun) : chart
      case 'comp':
        return args.comp ? chart.assign(args.head, args.head.frame(chart).type in ['FORGET', 'DISCOVER', 'AMAZE'] ? 'theme' : 'question', args.comp) : chart
      case 'question':
        return args.questioned ? chart.assign(args.situation, 'questioned', args.questioned) : chart
      case 'comeScalarly':
        return args.order ? chart.assign(args.verb, 'type', 'COME_SCALARLY').assign(args.verb, 'order', args.order) : chart
      case 'questionVariants':
        return args.variant ? chart.assign(args.questioned, 'variant', args.variant) : chart
      case 'whatA':
        return args.time ? chart.assign(args.situation, 'time', args.time) : chart
      case 'possessive':
        chart = args.conj ? _apply(chart, 'possessive', args.conj) : chart //todo generic conj handling
        return args.head.frame(chart).type ? chart.assign(args.head, 'arg1', args.possessor) : chart
      case 'control':
        return args.slave ? chart.assign(args.head, 'theme', args.slave) : chart
    }

    return chart
  }

  ParsingState apply(Map newArgs = [:], String name, Closure init = noInit) {
    def args = constructions.get(name, [:]) + newArgs
    def oldInit = args.get('init', noInit)
    def newInit = { ParsingState state -> init(oldInit(state)) }
    args.init = newInit
    def newConstructions = constructions + [(name): args]
    ParsingState newState = newInit(this).clone(constructions: newConstructions)
    return newState.withChart(newState._apply(newState.chart, name, args))
  }


}