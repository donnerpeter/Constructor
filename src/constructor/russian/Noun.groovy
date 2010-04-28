package constructor.russian

import constructor.Construction
import constructor.Descriptor
import constructor.PhraseConstruction
import constructor.ParsingContext
import constructor.PhraseDescriptor

/**
 * @author peter
 */
class Noun extends Descriptor {
  def _case
  def agr

  def Noun(String name, _case, String... agr) {
    super(name)
    this._case = _case
    this.agr = agr
    famous()
  }

  PhraseDescriptor makePhrase(Construction c, ParsingContext ctx) {
    def existing = ctx.findBefore(c, [PhraseConstruction.PHRASE, _case], false)
    if (!existing || ctx.strongUsages(existing, PhraseConstruction.HEAD.name)) {
      return new PhraseDescriptor("NP").aka("noun", name, _case).aka(agr)
    }
    return null
  }

  boolean activate(Construction c, ParsingContext ctx) {
    if (!ctx.strongUsages(c, PhraseConstruction.HEAD.name)) {
      def existing = ctx.findBefore(c, [PhraseConstruction.PHRASE, _case], false)
      if (!existing || ctx.strongUsages(existing, PhraseConstruction.HEAD.name)) {
        def frame = new PhraseDescriptor("NP").aka("noun", name, _case).aka(agr).build([], ctx.cloud)
        ctx.demote(c)
        ctx.addConstruction(frame)
        ctx.addConstruction(PhraseConstruction.HEAD.build([frame, c], ctx.cloud))
        return
      }
    }

    if ("nominative" == _case) {
      def clause = ctx.findBefore(c, "clause", false)
      if (clause && ctx.allUsages(clause, RussianLexicon.subject.name)) {
        ctx.markFinished(clause)
      }
    }

    return super.activate(c, ctx)
  }

  Noun nounObj(String slot) {
    expect(["_", "genitive"], new Descriptor("NounObj").consumes(1).semantics { it[0][slot] = it[1]; it[0] } )
  }
}
