package cons3
import groovy.transform.EqualsAndHashCode
import groovy.transform.TupleConstructor
/**
 * @author peter
 */
@TupleConstructor
class Domination {
  ParsingState self
  ParsingState other

  private Pair<Frame, Frame> findPair(Closure condition) {
    for (frame in self.chart.frames) {
      Frame alternative = frame.unifiedVar.frame(other.chart)
      if (condition(frame, alternative)) {
        return new Pair(frame, alternative)
      }
    }
    return null
  }

  private static List<Problem> getProblems(ParsingState state) {
    def result = []
    for (frame in state.chart.frames) {
      if (frame.allAssignments('opinion_of').size() > 1) {
        result << problem(frame, 'several accordingTo assignments')
      }
      if (frame.type == 'CASHIER' && frame.f('arg1')?.flatten()?.any { it.type && it.type != 'SHOP' }) {
        result << problem(frame, 'cashier of something strange')
      }
      if (frame.f('member') && frame.flatten().any { it.s('number') == 'true' } && frame.flatten().any { !it.s('number') }) {
        result << problem(frame, 'number and non-number conjunction')
      }
      if (frame.type == 'COME_SCALARLY' && frame.f('arg1')?.canBeHuman() && frame.f('anchor')?.definitelyNonHuman()) {
        result << problem(frame, 'human comes after non-human')
      }
      if (frame.type == 'COME_SCALARLY' && frame.f('arg1')?.definitelyNonHuman() && frame.f('anchor')?.type && frame.f('anchor')?.canBeHuman()) {
        result << problem(frame, 'non-human comes after human')
      }
    }

    result
  }

  private static Problem problem(Frame frame, String description) {
    return new Problem(frame, description)
  }

  boolean dominatesSemantically() {
    if (findPair { verb, alternative ->
      verb.type == 'GO' && alternative.type == 'COME_SCALARLY' && alternative.f('anchor')?.type && alternative.f('anchor')?.canBeHuman()
    }) {
      return true
    }
    if (findPair { verb, alternative ->
      verb.type == 'COME_SCALARLY' && alternative.type == 'GO' && verb.f('anchor')?.type && !verb.f('anchor').canBeHuman()
    }) {
      return true
    }

    def problems1 = getProblems(self)
    def problems2 = getProblems(other)
    if (problems1.size() < problems2.size()) {
      return true
    }

    for (verb in self.chart.frames.findAll { it.humanAction }) {
      def alternative = verb.unifiedVar.frame(other.chart)

      def mySubj = verb.f('arg1')
      def otherSubj = alternative.f('arg1')
      boolean meHuman = !mySubj?.definitelyNonHuman()
      def otherHuman = !otherSubj?.definitelyNonHuman()
      if (meHuman && !otherHuman && mySubj?.type && otherSubj?.type) {
        return meHuman
      }

    }

    return false
  }

}
@TupleConstructor
@EqualsAndHashCode
class Problem {
  Frame frame
  String description

  @Override
  public String toString() {
    return "$description: $frame";
  }
}