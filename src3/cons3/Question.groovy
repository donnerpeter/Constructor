package cons3

/**
 * @author peter
 */
class Question extends Construction implements RussianConstructions {

  def Question() {
    super('question', ['questioned', 'frame', 'content'])
  }

  @Override
  ParsingState applySemantics(Mite mite, ParsingState state) {
    def args = mite.contents
    state = state.assign(args.frame, 'content', args.content)
    return state.assign(args.frame, 'type', 'question').assign(args.frame, 'questioned', args.questioned)
  }

  @Override
  Collection<Mite> enrichUpdate(Mite self, List<Mite> contribution, ParsingState state) {
    def dash = contribution.find { it.cxt == questionVariants && it.contents.hasDash }
    def questioned = self.contents.questioned
    if (questioned && dash) {
      return [dash.unify(new Mite(questionVariants, questioned:questioned))]
    }
    return self.unifyWherePossible(contribution)
  }

  @Override
  List<ParsingState> showPreviousHierarchy(Mite self, ParsingState state, boolean up) {
    state.findState(self, 'frame')?.hierarchy
  }

  @Override
  boolean isMiteSatisfied(Mite mite) {
    return mite.contents.content && mite.contents.frame
  }
}
