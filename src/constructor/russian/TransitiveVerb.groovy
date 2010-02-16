package constructor.russian

import constructor.Descriptor
import constructor.Construction
import constructor.ParsingContext

/**
 * @author peter
 */
class TransitiveVerb extends Descriptor {
  def agr

  def TransitiveVerb(name, agr) {
    super(name);
    this.agr = agr
    aka("clause")
    famous()
  }

  boolean activate(Construction cur, ParsingContext ctx) {
    if (ctx.cloud.finished.contains(cur)) {
      def oldS = ctx.usages(cur, RussianLexicon.subject.name)
      def oldO = ctx.usages(cur, RussianLexicon.object.name)
      if (!oldS || !oldO) {
        (oldS + oldO).each { ctx.cloud.weak << it }

        def s = ctx.findAll(cur, ["nominative", agr])
        if (s.size() == 1) {
          ctx.addConstruction(RussianLexicon.subject.build([cur, s[0]]))
        }
        def o = ctx.findAll(cur, "accusative")
        if (o.size() == 1) {
          ctx.addConstruction(RussianLexicon.object.build([cur, o[0]]))
        }
        s = ctx.findAll(cur, ["nominative", agr])
        if (s.size() == 1) {
          ctx.addConstruction(RussianLexicon.subject.build([cur, s[0]]))
        }

      }
      return
    }

    def s = ctx.findBefore(cur, ["nominative", agr])
    if (!s) s = ctx.findAfter(cur, ["nominative", agr])
    if (s) ctx.addConstruction(RussianLexicon.subject.build([cur, s]))

    def o = ctx.findAfter(cur, "accusative")
    if (!o) o = ctx.findBefore(cur, "accusative")
    if (o) ctx.addConstruction(RussianLexicon.object.build([cur, o]))

    return super.activate(cur, ctx);
  }

/*
  def List<Construction> incompatible(Construction my, ParsingContext ctx) {
    def pivot = ctx.cloud.ranges[my].fromInt

    def subjBefore = []
    def subjAfter = []
    def objBefore = []
    def objAfter = []
    ctx.usages(my, "SubjPred").each { (ctx.cloud.ranges[it.args[1]].fromInt < pivot ? subjBefore : subjAfter) << it }
    ctx.usages(my, "Obj").each { (ctx.cloud.ranges[it.args[1]].fromInt < pivot ? objBefore : objAfter) << it }

    return super.incompatible(my, ctx);
  }
*/


}
