package cons3

import groovy.transform.TupleConstructor
import org.pcollections.ConsPStack
import org.pcollections.PStack

import static cons3.RussianConstructions.*

/**
 * @author peter
 */
@TupleConstructor
class EllipsisInterceptor extends Interceptor {
  final ParsingState beforeDash

  ParsingState intercept(List<Mite> constructions, ParsingState state, Function2<List<Mite>, ParsingState, ParsingState> base) {
    Mite self = state.visibleMites[clauseEllipsis].find { state.network.isChosen(it) && it.contents.interceptor == (Interceptor) this }
    final ParsingState prevClause = self.contents.prevHistory
    if (!prevClause || state[clauseEllipsis].finished || state.visibleMites[questionVariants]?.find { it.contents.questioned }) {
      return base(constructions, state)
    }
    if (!canPrecedeEllipsis(beforeDash)) {
      return base(constructions, state)
    }

    Update update = new Update(constructions)

    List<ParsingState> old = prevClause.history
    def endPrecedent = old.findIndexOf { ParsingState it -> Similarity.areSimilar(it.contribution, update.mites) }
    int startPrecedent = old.findIndexOf(endPrecedent + 1) { ParsingState it -> canPrecedeEllipsis(it) }
    if (startPrecedent < 0 || endPrecedent < 0) {
      return base(constructions, state)
    }

    PStack<ParsingState> precedent = ConsPStack.from(old[0..startPrecedent-1].reverse())

    Map<Variable, Variable> mapping = buildMapping(old[startPrecedent], precedent, update)

    def ellipsis = new Variable()
    state = state.assign(ellipsis, 'type', 'ellipsis')

    Map<Object, Object> xorMapping = [:]
    for (i in 0..<precedent.size()) {
      if (i == precedent.size() - endPrecedent - 1) {
        state = base(constructions, state)
      } else {
        List<Mite> mapped = applyMapping(precedent[i], mapping, xorMapping)
        state = base(mapped, state.startMeta(ellipsis)).finishMeta(ellipsis)
      }
    }

    return state.apply((clauseEllipsis):[finished:true])
  }

  private static boolean canPrecedeEllipsis(ParsingState st) {
    return st[accordingTo]?.content && st.linkUp || st.ownMites.find { it.cxt == accordingTo && it.contents.content }
  }

  static List<Mite> applyMapping(ParsingState oldContribution, Map<Variable, Variable> mapping, Map<Object, Object> xorMapping) {
    List<Mite> newContribution = []
    for (mite in oldContribution.contribution) {
      def newArgs = [:]
      mite.contents.each { k, v ->
        newArgs[k] = v instanceof Variable ? mapping[(Variable) v] : v
      }
      if (mite.contents.xor) {
        newArgs.xor = ((Set) mite.contents.xor).collect { xorMapping.get(it, new TokenWrapper(it)) } as LinkedHashSet
      }
      newContribution = newContribution + mite.cxt.call(newArgs)
    }
    return newContribution
  }

  static Map<Variable, Variable> buildMapping(ParsingState antecedent, List<ParsingState> template, Update update) {
    Map<Variable, Variable> mapping = [:]
    antecedent.miteList.each { mite ->
      mite.contents.values().each { val ->
        if (val instanceof Variable && !mapping[val]) {
          def newVar = new Variable()
          mapping[val.base] = newVar
          mapping[val.base.lightVar] = newVar.lightVar
        }
      }
    }
    template.each { ParsingState oldContribution ->
      oldContribution.contribution.each { mite ->
        mite.contents.each { k, v ->
          if (v instanceof Variable && !mapping[v]) {
            List<Mite> candidateMites = update.mites.findAll { it.isSimilarTo(mite) }
            List values = candidateMites.collect { it.contents[k] }
            Variable newVar = values[0] ?: mapping[v] ?: new Variable()
            mapping[v.base] = newVar.base
            mapping[v.base.lightVar] = newVar.base.lightVar
          }
        }
      }
    }
    return mapping
  }

}