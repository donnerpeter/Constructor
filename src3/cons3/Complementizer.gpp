package cons3

/**
 * @author peter
 */
class Complementizer extends Construction implements RussianConstructions {

  def Complementizer() {
    super('complementizer', ['frame', 'content'])
  }

  @Override
  ParsingState applySemantics(Mite mite, ParsingState state) {
    def args = mite.contents
    state.assign(args.frame, 'type', 'fact').assign(args.frame, 'content', args.content)
  }

  @Override
  List<ParsingState> showPreviousHierarchy(Mite self, ParsingState state, boolean up) {
    up ? state.findState(self, 'frame')?.hierarchy : null
  }

  @Override
  boolean isMiteSatisfied(Mite mite) {
    return mite.contents.content && mite.contents.frame
  }
}
