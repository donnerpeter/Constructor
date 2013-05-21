package cons3

import static cons3.RussianConstructions.*

/**
 * @author peter
 */
class Similarity {
  private static final ArrayList<Construction> importantForSimilarity =
    [nom, acc, gen, dat, instr, prep, poDat, nomSubject,
            possessive, parenthetical, comeScalarly, elaboration, declOrQuestionComp, question]

  static boolean areSimilar(List<Mite> c1, List<Mite> c2) {
    def common = c1.findAll { m1 -> m1.cxt in importantForSimilarity && m1.findSimilar(c2) } as List<Mite>
    common = common.findAll { !(it.cxt == possessive && it.contents.head) }

    if (!common) {
      return false
    }

    def nq1 = c1.findAll { it.cxt == numQuantifier }
    def nq2 = c2.findAll { it.cxt == numQuantifier }
    if (nq1 && nq2 && nq1.every { m1 -> nq2.every { m2 -> !m1.isSimilarTo(m2) } }) {
      return false
    }

    return true
  }

}