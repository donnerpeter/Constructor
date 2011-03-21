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

  ParsingState withSituation(Situation situation) { clearConstructions().clone(situation:situation) }

  Variable newFrame() {
    new Variable(situation)
  }

  ParsingState assign(Variable var, String property, def value) {
    clone(chart: chart.assign(var, property, value))
  }


  private static ParsingState _apply(ParsingState state, String name, Map args) {
    switch (name) {
      case 'adjective':
        return state.assign(args.nounFrame, args.rel, args.val)
      case 'nom':
        return args.head.frame(state.chart).type ? state.assign(args.head, 'arg1', args.noun) : state
      case 'acc':
        return args.head.frame(state.chart).type && args.noun ? state.assign(args.head, 'arg2', args.noun) : state
      case 'instr':
        if (args.save && args.noun) {
          return state.clone(constructions:args.save).apply('sInstr', noun:args.noun)
        }
        return state
      case 'sInstr':
        return args.noun ? state.assign(args.head, 'experiencer', args.noun) : state
      case 'poDat':
        return args.noun ? state.assign(args.head, 'topic', args.noun) : state
      case 'nounGen':
        return args.noun ? state.assign(args.head, 'criterion', args.noun) : state
      case 'kDat':
        return args.noun ? state.assign(args.head, 'goal', args.noun) : state
      case 'comp':
        return args.comp ? state.assign(args.head, args.head.frame(state.chart).type in ['FORGET', 'DISCOVER', 'AMAZE'] ? 'theme' : 'question', args.comp) : state
      case 'question':
        return args.questioned ? state.assign(args.situation, 'questioned', args.questioned) : state
      case 'comeScalarly':
        return args.order ? state.assign(args.verb, 'type', 'COME_SCALARLY').assign(args.verb, 'order', args.order) : state
      case 'questionVariants':
        return args.variant ? state.assign(args.questioned, 'variant', args.variant) : state
      case 'whatA':
        return args.time ? state.assign(args.situation, 'time', args.time) : state
      case 'possessive':
        state = args.conj ? _apply(state, 'possessive', args.conj) : state //todo generic conj handling
        return args.head.frame(state.chart).type ? state.assign(args.head, 'arg1', args.possessor) : state
      case 'control':
        return args.slave ? state.assign(args.head, 'theme', args.slave) : state
    }

    return state
  }

  ParsingState apply(Map newArgs = [:], String name, Closure init = noInit) {
    def args = constructions.get(name, [:]) + newArgs
    def oldInit = args.get('init', noInit)
    def newInit = { ParsingState state -> init(oldInit(state)) }
    args.init = newInit
    def newConstructions = constructions + [(name): args]
    ParsingState newState = newInit(this).clone(constructions: newConstructions)
    return _apply(newState, name, args)
  }


  ParsingState clearConstructions() {
    return clone(constructions: [:])
  }
}