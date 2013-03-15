package cons3

import org.pcollections.ConsPStack
import org.pcollections.PStack

/**
 * @author peter
 */
class Update {
  PStack<Mite> mites

  Update(Mite... mites) {
    this(mites as List)
  }
  Update(List<Mite> mites) {
    this.mites = mites instanceof PStack ? mites : ConsPStack.from(mites.reverse())
  }

  Update plus(Update update) {
    return addMites(Util.reverse(update.mites) as Mite[])
  }
  Update plus(Map<Construction, Map> constructions) {
    return addMites((constructions.keySet() as List<Construction>).collect { it(constructions[it]) } as Mite[])
  }

  Update addCxt(Map newArgs, Construction name) {
    return addMites(name(newArgs))
  }

  Update xor(Update another) { xor(Util.reverse(another.mites) as Mite[]) }
  Update xor(Mite... mites2) {
    Tokens t = new Tokens()
    List<Mite> mites1 = Util.reverse(this.mites)
    char c = 'a'
    Map<Mite, LinkedHashSet<Object>> newXors = [:]
    for (m1 in mites1) {
      for (m2 in mites2) {
        def token = (t.getProperty(c as String) as Set).iterator().next()
        newXors.get(m1, new LinkedHashSet()).add token
        newXors.get(m2, new LinkedHashSet()).add token
        c++
      }
    }
    List<Mite> newMites = []
    def addXors = { Mite it ->
      LinkedHashSet oldXors = it.contents.xor ?: new LinkedHashSet()
      LinkedHashSet xors = new LinkedHashSet(oldXors)
      for (x in newXors[it]) {
        xors << x
      }
      if (it.atom) {
        new Mite(it.contents + [xor:xors], it.cxt, it.unifications)
      } else {
        it.unify(it.cxt.call(xor:xors))
      }
    }
    newMites.addAll mites1.collect(addXors)
    newMites.addAll mites2.collect(addXors)
    return new Update(newMites)
  }

  Update addMites(Mite... mites) {
    return new Update(this.mites.plusAll(mites.findAll { !(it in this.mites) }))
  }

  ParsingState apply(ParsingState state) {
    return state.apply(Util.reverse(mites))
  }

  Update optional() {
    xor(Construction.noArg(reason:Util.reverse(mites)[0].cxt.name))
  }

}
