package cons3

/**
 * @author peter
 */
class ParsingState {
  static final Closure noInit = { it }

  final Chart chart
  final Situation situation
  Map<Construction, Map> constructions = [:]

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

  Variable newVariable() {
    new Variable(situation)
  }

  ParsingState assign(Variable var, String property, def value) {
    clone(chart: chart.assign(var, property, value))
  }

  Map getAt(Construction construction) {
    constructions[construction]
  }

  ParsingState apply(Map newArgs = [:], Construction name, Closure init = null) {
    return addCtx(newArgs, name, init).applyAll(name)
  }

  ParsingState addCtx(Map newArgs, Construction name, Closure init = null) {
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

  ParsingState inhibit(Construction name) {
    def state = satisfied(name)

    def freed = [] as Set
    state.constructions.keySet().each {
      if (contradict(it, name) || contradict(name, it)) {
        freed << it
      }
    }
    return state.applyAll(freed as Construction[])
  }

  ParsingState satisfied(Construction name) {
    def newConstructions = new HashMap(constructions)
    newConstructions.remove(name)
    return clone(constructions: newConstructions)
  }

  ParsingState applyAll(Construction... names) {
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
        result = it.action.call(init(result), args)
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

  boolean contradict(Construction name1, Construction name2) {
    if (name1.name == 'nom' && name2.name == 'acc' && constructions[name1].noun == constructions[name2].noun && constructions[name1].noun) {
      return true
    }

    return false
  }
}
