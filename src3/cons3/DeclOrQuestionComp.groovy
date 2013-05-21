package cons3

/**
 * @author peter
 */
class DeclOrQuestionComp extends Construction implements RussianConstructions {

  def DeclOrQuestionComp() {
    super('declOrQuestionComp', ['head', 'comp', 'rel'])
  }

  @Override
  ParsingState applySemantics(Mite mite, ParsingState state) {
    def args = mite.contents
    state.assign(args.head, (String)args.rel, args.comp)
  }

  Collection<Mite> enrichUpdate(Mite mite, List<Mite> contribution, ParsingState state) {
    if (!mite.contents.head) {
      return []
    }

    if (contribution.find { it.cxt == comma }) {
      def xor = contribution.find { it.cxt == seq }.contents.xor
      if (!mite.contents.comma1) {
        return [mite.unify(comma1:true, beforeComma1:state, xor:xor)]
      }
      else if (!mite.contents.comma2 && mite.contents.comp) {
        return [mite.unify(comma2:true, xor:xor)]
      }
    }

    if (!mite.contents.comp && contribution.find { it.cxt == declOrQuestionComp && it.v('comp') }) {
      return mite.unifyWherePossible(contribution)
    }

    return []
  }

  @Override
  List<ParsingState> showPreviousHierarchy(Mite self, ParsingState state, boolean up) {
    if (self.contents.comma2) {
      return ((ParsingState) self.contents.beforeComma1).hierarchy
    }
    else if (self.contents.comma1) {
      return [ParsingState.EMPTY]
    }
  }

  @Override
  boolean isMiteSatisfied(Mite mite) {
    return mite.contents.head && mite.contents.comp
  }
}