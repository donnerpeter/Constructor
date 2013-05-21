package cons3

import static cons3.RussianConstructions.*

/**
 * @author peter
 */
class NumQuantifier {

  static Collection<Mite> enrichUpdate(Mite self, List<Mite> contribution, ParsingState state) {
    List<Mite> result = []
    if (self.contents.num) {
      def args = self.contents
      for (mite in contribution) {
        if (mite.cxt == args.innerCase) {
          Variable noun = mite.contents.noun
          result.addAll(new Mite(noun:noun, xor:mite.contents.xor, numQuantifier).unifyWherePossible(state))
        }
      }
    }
    return self.unifyWherePossible(contribution) + result
  }

}
