package cons3

/**
 * @author peter
 */
class Filler extends Construction implements RussianConstructions {

  def Filler() {
    super('filler')
  }

  @Override
  ParsingState applySemantics(Mite mite, ParsingState state) { state }

  @Override
  Collection<Mite> enrichUpdate(Mite mite, List<Mite> contribution, ParsingState state) {
    def whState = state.findState(mite)
    while (true) {
      def prev = whState.prevVisibleState
      if (prev.empty || prev.ownMites.find { it.cxt == comma }) {
        break
      }
      whState = prev
    }
    List<Mite> fillerContent = whState.contribution.findAll { it.cxt in [nom, acc, oPrep] }
    if (!mite.contents.filled && contribution.find { it.unifyWherePossible(fillerContent) }) {
      return [mite.unify(filled:true)]
    }
    return []

  }

  @Override
  boolean isMiteSatisfied(Mite mite) {
    return mite.contents.filled
  }
}
