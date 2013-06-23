package cons3

import cons4.*
import junit.framework.TestCase

class ParsingStateTest extends TestCase {
  def state = new ParsingState()
  def tokens = new Tokens()
  def v = new Vars()
  Construction cxt1 = new Construction()

  def word(Map args) {
    new Mite(cxt1, args, null, null, null)
  }

  ParsingState apply(List<Mite> mites) {
    return state = state.apply(mites as Mite[])
  }

  void "test add mite"() {
    def mite = word(word:'hello')
    state = state.apply(mite)
    assert state.active == [mite] as Set
  }

  void "test candidate sets in one column"() {
    def mites = [word(word:'x1')] + Cons4Package.xor([word(word:'x2')], [word(word:'x3')])
    state = apply(mites)
    assert state.network.columns[0].candidateSets.size == 2
    assert mites[0] in state.active
    assert mites[1] in state.active
    assert !(mites[2] in state.active)
  }

  def t(String s) {
    def result = [] as LinkedHashSet
    for (c in s) {
      result << tokens[c]
    }
    return result
  }

  void "test free candidate sets"() {
    def m0 = [word(word:'x0'), word(word:'x1', xor:t('ad')), word(word:'x2', xor:t('cd')), word(word:'x3', xor:t('d'))]
    def m1 = [word(word:'x4'), word(word:'x5', xor:t('ab'))]
    def m2 = [word(word:'x6'), word(word:'x7', xor:t('b')), word(word:'x8', xor:t('c'))]
    apply(m0)
    apply(m1)
    apply(m2)
    assert m0[0] in state.active
    assert m0[2] in state.active

    Map<Integer, List<CandidateSet>> freeSets = state.getFreeCandidateSets([1, 2])
    assert freeSets[1] == state.network.columns[1].candidateSets
    assert freeSets[2].every { !(m2[2] in it.set) }

  }


}
