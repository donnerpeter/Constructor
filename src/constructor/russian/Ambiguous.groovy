package constructor.russian

import constructor.Descriptor
import constructor.Construction
import constructor.ParsingContext

/**
 * @author peter
 */
class Ambiguous extends Descriptor {
  List<Descriptor> variants = []

  def Ambiguous(name) {
    super(name);
    variants << new Descriptor(name) {

      boolean activate(Construction c, ParsingContext ctx) {
        def clause = ctx.findBefore(c, "clause")
        if (clause && ctx.usages(clause, RussianLexicon.subject.name)) {
          ctx.markFinished(clause)
        }
        return true
      }

    }.aka("noun", "nominative", "3sg").famous()
    variants << new Descriptor(name).aka("noun", "accusative", "3sg").famous()
    famous()
  }

  def ping(message, ParsingContext ctx) {
    def pinged = variants.findAll { it.ping(message, ctx) }
    if (!pinged) return false
    if (pinged.size() == variants.size()) return true

    def intersect = usedVariants(ctx).intersect(pinged)
    return intersect.size() > 0
  }

  def usedVariants(ParsingContext ctx) {
    Set<Descriptor> result = variants as Set
    def strongUsages = ctx.strongUsages(ctx.relativeTo, [])
    strongUsages.each { usg ->
      def msg = usg.descr.argumentPing(usg.args.indexOf(ctx.relativeTo))

      result -= result.findAll { var -> !var.ping(msg, ctx) }
    }
    return result
  }

  boolean activate(Construction c, ParsingContext ctx) {
    def happy = true
    usedVariants(ctx).each {
      happy &= it.activate(c, ctx)
    }
    return happy
  }


}
