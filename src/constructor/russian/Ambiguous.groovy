package constructor.russian

import constructor.Descriptor
import constructor.Construction
import constructor.ParsingContext
import constructor.Cloud

/**
 * @author peter
 */
class Ambiguous extends Descriptor {

  def Ambiguous(name) {
    super(name);
    aka('3sg')
    famous()
  }

  def ping(message, ParsingContext ctx) {
    //todo generalize ambiguous words
    def subjUsages = ctx.strongUsages(ctx.relativeTo, "SubjPred")
    def objUsages = ctx.strongUsages(ctx.relativeTo, "Obj")
    ctx.strongUsages(ctx.relativeTo, "AdjNoun").each { usg ->
      if (usg.args[0].isAccepted("nom", ctx.cloud)) subjUsages << usg
      if (usg.args[0].isAccepted("acc", ctx.cloud)) objUsages << usg
    }
    if (message == "nominative" && !objUsages) {
      return true
    }
    if (message == "accusative" && !subjUsages) {
      return true
    }

    return super.ping(message, ctx);
  }

  boolean activate(Construction c, ParsingContext ctx) {
    if (ping("nominative", ctx) && !ping("accusative", ctx)) {
      def clause = ctx.findBefore(c, "clause")
      if (clause && ctx.usages(clause, "SubjPred")) {
        ctx.markFinished(clause)
      }
    }

    return super.activate(c, ctx);
  }


}
