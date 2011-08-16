package cons3

/**
 * @author peter
 */
class EllipsisInterceptor implements Interceptor, RussianConstructions {

  ParsingState intercept(Map<Construction, Map<String, Object>> constructions, ParsingState state, Function2<Map<Construction, Map<String, Object>>, ParsingState, ParsingState> base) {
    Update update = new Update(FLinkedMap.emptyMap)
    for (cxt in constructions.keySet()) {
      update = update.addCxt(constructions[cxt], cxt)
    }

    Map<Variable, Variable> mapping = state[clauseEllipsis].mapping
    List<Contribution> prevConstructions = state[clauseEllipsis].remaining
    def index = prevConstructions.findIndexOf { Contribution it -> Parser.areSimilar(it.apps, update.map) }
    if (index >= 0) {
      state = state.satisfied(clauseEllipsis)
      prevConstructions.each { Contribution oldContribution ->
        oldContribution.apps.each { cxt, upd ->
          upd.each { k, v ->
            if (v instanceof Variable) {
              def nv = update.map[cxt]?.get(k) ?: mapping.get(v)
              if (!nv) {
                nv = new Variable()
              }
              mapping[v] = nv
            }
          }
        }
      }

      def ellipsis = new Variable()
      state = state.assign(ellipsis, 'type', 'ellipsis').startMeta(ellipsis)

      for (i in 0..<prevConstructions.size()) {
        if (i == index) continue

        Contribution oldContribution = prevConstructions[i]
        //todo a normal condition for ellipsis participation
        if (!oldContribution.apps.keySet().intersect(Parser.importantForSimilarity - [parenthetical])) continue

        FLinkedMap newContribution = FLinkedMap.emptyMap
        for (cxt in oldContribution.apps.keySet()) {
          def newArgs = [:]
          oldContribution.apps[cxt].each { k, v ->
            newArgs[k] = v instanceof Variable ? mapping[(Variable) v] : v
          }
          newContribution = newContribution.putValue(cxt, newArgs)
        }
        state = base(newContribution.reverse(), state)
      }

      state = state.finishMeta(ellipsis)
    }
    return base(constructions, state)
  }
}
