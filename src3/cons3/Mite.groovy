package cons3

import org.pcollections.Empty
import org.pcollections.PStack

/**
 * @author peter
 */
class Mite {
  final Construction cxt
  final FLinkedMap contents
  final PStack<Pair<Variable, Variable>> unifications
  final Mite src1, src2
  final List<Mite> primaries
  final boolean executable

  Mite(Map contents, Construction cxt, PStack<Pair<Variable, Variable>> unifications = Empty.stack(), Mite src1 = null, Mite src2 = null) {
    this.cxt = cxt
    this.contents = contents instanceof FLinkedMap ? contents : FLinkedMap.fromMapReverse(contents)
    this.unifications = unifications
    this.src1 = src1
    this.src2 = src2
    primaries = !src1 ? [this] : src1.primaries + src2.primaries
    executable = cxt.isExecutable(this)
  }

  LinkedHashSet getXor() {
    return contents.xor ?: new LinkedHashSet()
  }

  public static Map unify(Map oldArgs, Map newArgs) {
    def args = new HashMap()
    if (oldArgs) args.putAll(oldArgs)
    if (newArgs) {
      for (key in newArgs.keySet()) {
        if (oldArgs[key] instanceof Variable && !((Variable) oldArgs[key]).light) {
          continue
        }
        args[key] = newArgs[key]
      }
    }

    Collection xor = mergeXor(oldArgs, newArgs)
    if (xor) {
      args.xor = xor
    }

    return args
  }

  boolean isPartOf(Mite another) {
    return another == this || !another.atom && (isPartOf(another.src1) || isPartOf(another.src2))
  }

  static Collection mergeXor(Map oldArgs, Map newArgs) {
    Set xor1 = oldArgs?.xor ?: Collections.emptySet()
    Set xor2 = newArgs?.xor ?: Collections.emptySet()
    if (!xor1 && !xor2) {
      return null
    }

    def xor = new LinkedHashSet()
    xor.addAll xor1
    xor.addAll xor2
    return xor
  }

  static boolean overwrites(Map before, Map newArgs) {
    if (before == null) {
      return false
    }
    for (arg in before.keySet().intersect(newArgs.keySet())) {
      if (arg == 'xor') continue

      def val1 = before[arg]
      def val2 = newArgs[arg]
      if (val1 != val2 && val1 != null && val2 != null && !areUnifiableVars(val1, val2)) {
        return true
      }
    }
    return false
  }

  private static boolean areUnifiableVars(val1, val2) {
    return val1 != val2 && val1 instanceof Variable && val2 instanceof Variable && (val1.light || val2.light) && val1.base != val2.base
  }

  @Override
  public String toString() {
    return "$cxt$contents" +
            (unifications ? unifications.collect { "<$it.first=$it.second>" }.join("") : '')
  }

  boolean isSimilarTo(Mite another) {
    return cxt == another.cxt && contents.keySet() - 'xor' == another.contents.keySet() - 'xor'
  }

  Mite findSimilar(List<Mite> mites) {
    mites.find { isSimilarTo(it) }
  }

  Mite unify(Map args) {
    return unify(cxt(args))
  }

  Mite unify(Mite another) {
    def before = contents
    def newArgs = another.contents
    assert !overwrites(before, newArgs)
    def newUnifications = unifications.plusAll(another.unifications)
    for (arg in before.keySet().intersect(newArgs.keySet())) {
      def val1 = before[arg]
      def val2 = newArgs[arg]
      if (areUnifiableVars(val1, val2)) {
        def pair = new Pair(val1, val2)
        if (!(pair in newUnifications)) {
          newUnifications = newUnifications + pair
        }
      }
    }

    return new Mite(unify(before, newArgs), cxt, newUnifications, this, another)
  }

  boolean isAtom() {
    src1 == null
  }

  List<Mite> unifyWherePossible(ParsingState state) {
    return unifyWherePossible(state.visibleMites[cxt], false, true)
  }

  List<Mite> unifyWherePossible(Collection<Mite> mites, boolean before = true, boolean includeSelf = false) {
    List<Mite> result = []
    if (includeSelf) {
      result << this
    }
    if (mites) {
      result.addAll mites.
              findAll { isUnifiable(it) }.
              collect { before ? this.unify(it) : it.unify((Mite)this) }
    }
    return result
  }

  boolean isUnifiable(Mite with) {
    return with.cxt == cxt && !overwrites(contents, with.contents)
  }

  boolean isSatisfied() {
    return cxt.isMiteSatisfied(this)
  }

  Mite getLastAtom() {
    return primaries[primaries.size() - 1]
  }

  Mite getFirstAtom() {
    return primaries[0]
  }

  Variable v(String attr) {
    return (Variable) contents[attr]
  }
}