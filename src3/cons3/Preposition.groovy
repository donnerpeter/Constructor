package cons3

import groovy.transform.CompileStatic

import static cons3.RussianConstructions.*

/**
 * @author peter
 */
@CompileStatic
class Preposition extends Construction {
  def Preposition() {
    super('preposition')
  }

  @Override
  List<ParsingState> showPreviousHierarchy(Mite self, ParsingState state, boolean up) {
    return [ParsingState.EMPTY]
  }

  @Override
  Collection<Mite> enrichUpdate(Mite self, List<Mite> update, ParsingState state) {
    def result = []
    for (newMite in update) {
      if (newMite.cxt == boxedForPreposition && newMite.contents.prep == self.contents.prep) {
        for (boxed in (List<Mite>) newMite.contents.boxed) {
          result << new Mite(boxed.contents + [head:self.contents.head], boxed.cxt)
        }
      }
    }
    if (result) {
      def prepState = state.findState(self)
      result.addAll(prepState.prevState.enrichUpdate(result, state))
    }
    List<Mite> refl = update.findAll { Mite mite -> mite.cxt == reflexiveHolder } as List
    if (refl) {
      result.addAll(state.findState(self).prevState.enrichUpdate(refl, state))
    }
    result

  }
}