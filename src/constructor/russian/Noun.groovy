package constructor.russian

import constructor.Construction
import constructor.Descriptor
import constructor.ParsingContext

/**
 * @author peter
 */
class Noun extends Descriptor {
  def _case

  def Noun(name, _case) {
    super(name)
    this._case = _case
    aka("noun", _case)
    famous()
  }

  boolean activate(Construction c, ParsingContext ctx) {
    if ("nominative" == _case) {
      def clause = ctx.findBefore(c, "clause")
      if (clause && ctx.usages(clause, RussianLexicon.subject.name)) {
        ctx.markFinished(clause)
      }
    }

    return super.activate(c, ctx)
  }
}
