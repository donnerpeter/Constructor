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
        return args.head?.frame(state.chart)?.type ? state.assign(args.head, 'arg1', args.noun) : state
      case 'acc':
        return args.head?.frame(state.chart)?.type && args.noun ? state.assign(args.head, 'arg2', args.noun) : state
      case 'instr':
      case 'dat':
        return handleCase(name, state, args)
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
        return args.seq ? state.assign(args.questioned, 'variants', args.seq) : state
      case 'shortAdjCopula':
        if (args.noun && args.pred) {
          state = state.assign(args.noun, 'degree', args.pred)
        }
        return args.time ? state.assign(args.situation, 'time', args.time) : state
      case 'possessive':
        state = args.conj ? _apply(state, 'possessive', args.conj) : state //todo generic conj handling
        return args.head.frame(state.chart).type ? state.assign(args.head, 'arg1', args.possessor) : state
      case 'control':
        return args.slave ? state.assign(args.head, 'theme', args.slave) : state
      case 'seq':
        if (args.seq) {
          state = state.assign(args.seq, 'member', args.member)

          state.chart.allAssignments(state.situation).each {
            if (it.frame.var != args.seq && it.value instanceof Frame && it.value.var == args.member) {
              state = state.assign(it.frame.var, it.property, args.seq)
            }
          }

          if (args.conj) {
            state = state.assign(args.seq, 'conj', args.conj)
          }
        }
        return state
      case 'advObj':
        if (args.head && args.adv) {
          state = state.assign(args.head, 'arg2', args.adv)
        }
        return state
      case 'quotedName':
        if (args.finished) {
          state = state.assign(args.noun, 'name', args.name).satisfied('quotedName')
        }
        return state
    }

    return state
  }

  private static ParsingState handleCase(String caze, ParsingState state, Map args) {
    if (args.save && args.hasNoun) {
      return state.satisfied(caze).restore(args.save).apply(args.delegate, noun: args.noun)
    }
    return state
  }

  ParsingState apply(Map newArgs = [:], String name, Closure init = null) {
    return addCtx(newArgs, name, init).applyAll(name)
  }

  ParsingState addCtx(Map newArgs, String name, Closure init = null) {
    def oldArgs = constructions.get(name, [:])
    def replace = false
    for (cxt in oldArgs.keySet().intersect(newArgs.keySet())) {
      if (oldArgs[cxt] != newArgs[cxt] && oldArgs[cxt]) {
        replace = true
        oldArgs.clear()
        break
      }
    }

    def args = oldArgs + newArgs
    def oldInit = args.get('init', noInit)
    def newInit = init ? { ParsingState state -> init(oldInit(state)) } : oldInit
    args.init = newInit
    def newConstructions = constructions + [(name): args]
    return (replace ? inhibit(name) : this).clone(constructions: newConstructions)
  }

  ParsingState inhibit(String name) {
    def state = satisfied(name)

    def freed = [] as Set
    state.constructions.keySet().each {
      if (contradict(it, name) || contradict(name, it)) {
        freed << it
      }
    }
    return state.applyAll(freed as String[])
  }

  ParsingState satisfied(String name) {
    def newConstructions = new HashMap(constructions)
    newConstructions.remove(name)
    return clone(constructions: newConstructions)
  }

  ParsingState applyAll(String... names) {
    def hanging = [] as Set
    names.each { name1 ->
      names.each { name2 ->
        if (contradict(name1, name2)) {
          hanging << name1
          hanging << name2
        }
      }
    }

    ParsingState result = this
    def others = constructions.keySet() - (names as List)
    names.each { name1 ->
      others.each { name2 ->
        if (contradict(name1, name2) || contradict(name2, name1)) {
          result = result.inhibit(name2)
        }
      }
    }

    names.each {
      if (!(it in hanging)) {
        def args = constructions[it]
        Closure init = args.init
        result = _apply(init(result), it, args)
      }

    }
    return result
  }


  ParsingState clearConstructions() {
    return clone(constructions: [:])
  }

  ParsingState restore(Map saved) {
    clone(constructions: saved + this.constructions)
  }

  boolean contradict(String name1, String name2) {
    if (name1 == 'nom' && name2 == 'acc' && constructions[name1].noun == constructions[name2].noun && constructions[name1].noun) {
      return true
    }

    return false
  }
}
