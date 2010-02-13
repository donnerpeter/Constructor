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
      def oldS = ctx.usages(cur, "SubjPred")
      def oldO = ctx.usages(cur, "Obj")
      if (!oldS || !oldO) {
        (oldS + oldO).each { ctx.cloud.weak << it }

        def s = ctx.findAll(cur, ["nominative", agr])
        def o = ctx.findAll(cur, "accusative")
        if (s.size() == 1) {
          ctx.addConstruction(new Descriptor("SubjPred").consumes(1).build([cur, s[0]]))
          o.remove(s[0])
          s.clear()
        }
        if (o.size() == 1) {
          ctx.addConstruction(new Descriptor("Obj").consumes(1).build([cur, o[0]]))
          s.remove o[0]
          o.clear()
        }
        if (s.size() == 1) {
          ctx.addConstruction(new Descriptor("SubjPred").consumes(1).build([cur, s[0]]))
          o.remove(s[0])
          s.clear()
        }

      }
      return
    }

    def s = ctx.findBefore(cur, ["nominative", agr])
    if (!s) s = ctx.findAfter(cur, ["nominative", agr])
    if (s) ctx.addConstruction(new Descriptor("SubjPred").consumes(1).build([cur, s]))

    def o = ctx.findAfter(cur, "accusative")
    if (!o) o = ctx.findBefore(cur, "accusative")
    if (o) ctx.addConstruction(new Descriptor("Obj").consumes(1).build([cur, o]))

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
