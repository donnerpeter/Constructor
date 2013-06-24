package cons3

import cons4.*
import cons4.constructions.nom
import cons4.constructions.acc
import cons4.constructions.complementizer
import cons4.constructions.question
import cons4.constructions.control
import cons4.constructions.phrase

import junit.framework.TestCase

class ParsingStateTest extends TestCase {
  def state = new ParsingState()
  def tokens = new Tokens()
  def v = new Vars()
  Construction cxt1 = new Construction()

  def word(Map args) { mite(args, cxt1) }
  def nom(Map args) { mite(args, nom.instance$) }
  def acc(Map args) { mite(args, acc.instance$) }

  def mite(Map args, Construction cxt) { new Mite(cxt, args, null, null, null) }

  ParsingState apply(List<Mite> mites) {
    return state = state.apply(mites as Mite[])
  }

  void "test add mite"() {
    def mite = word(word:'hello')
    apply([mite])
    assert state.active == [mite] as Set
  }

  void "test add unhappy mite after happy"() {
    def m0 = word(word:'x1')
    def m1 = nom(noun:v.get(0))
    apply([m0, m1])
    assert state.active == [m0, m1] as Set
  }

  void "test add happy mite after happy and unhappy"() {
    def m0 = word(word:'x1')
    def m1 = nom(noun:v.get(0))
    def m2 = nom(noun:v.get(1), head:v.get(1))
    apply([m0, m1, m2])
    assert state.active == [m0, m1, m2] as Set
  }

  void "test add relation"() {
    apply([mite(phrase.instance$, kind:'verb', head:v.get(0))])
    def questionMite = state.network.getAllMites().find { it.cxt == question.instance$ }
    assert questionMite

    apply([mite(phrase.instance$, head:v.get(1).lv)])
    assert [0, 1] == state.network.getAllIndices(questionMite)
    assert questionMite in state.network.columns[0].mites
    assert questionMite in state.network.columns[1].mites
  }


  void "test merge mites"() {
    def m1 = nom(noun:v.get(0))
    def m2 = nom(head:v.get(1))
    apply([m1])
    apply([m2])

    assert state.network.columns[0].candidateSets.size == 2
    assert state.network.columns[1].candidateSets.size == 2

    def merged = m1.unify(m2)
    assert state.active == [merged] as Set
  }

  void "test that can recall order"() {
    def wh = v.get(0)
    def cp = v.get(1)
    def modal = v.get(2)
    def infinitive = v.get(3)
    def dObj = v.get(4)

    apply([mite(question.instance$, foo:'bar', xor:t('f')), mite(complementizer.instance$, foo:'bar', xor:t('f')), word(foo:'bar', xor:t('f'))])

    apply([nom(noun:wh, agr:'x', xor:t('ac')), acc(noun:wh, xor:t('ad')),
            mite(question.instance$, head:cp, questioned:wh, xor:t('b')), mite(complementizer.instance$, head:cp, xor:t('bcd'))])
    assert state.network.columns[1].candidateSets.size() == 6

    apply([nom(head:modal, agr:'y'), mite(control.instance$, head:modal),
            mite(question.instance$, content:modal, xor:t('e')), mite(complementizer.instance$, content:modal, xor:t('e'))])
    assert state.active.findAll { !it.happy }.collect { it.cxt } as Set == [nom.instance$, control.instance$] as Set

    apply([mite(control.instance$, slave:infinitive), acc(head:infinitive)])
    assert state.active.findAll { !it.happy }.collect { it.cxt } == [nom.instance$]

    apply([acc(noun:dObj)])
    assert state.active.findAll { !it.happy }.collect { it.cxt } == [nom.instance$]
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

}
