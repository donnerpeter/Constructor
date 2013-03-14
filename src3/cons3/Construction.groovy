package cons3

/**
 * @author peter
 */
class Construction {
  public static final Construction noArg = Util.emptyCxt('noArg')
  private Function2<ParsingState, Map, ParsingState> action =
    { state, args -> state } as Function2
  final String name
  private final List<String> requiredAttrs
  private Function4<Mite, ParsingState, Mite, Boolean, List<ParsingState>> showPrev =
    { mite, state, via, up -> null } as Function4
  private Function2<Mite, ParsingState, Boolean> wrongRoute =
    { mite, update -> false } as Function2
  private Function3<Mite, List<Mite>, ParsingState, Collection<Mite>> enrichAction = { mite, contribution, state ->
    return mite.unifyWherePossible(contribution)
  } as Function3
  private Predicate1<Mite> isSatisfied = { mite -> true } as Predicate1

  protected Construction(String name, List<String> requiredAttrs = []) {
    this.name = name
    this.requiredAttrs = requiredAttrs
  }

  ParsingState applySemantics(Mite mite, ParsingState state) { action(state, mite.contents) }

  Collection<Mite> enrichUpdate(Mite self, List<Mite> update, ParsingState state) { enrichAction(self, update, state) }

  List<ParsingState> showPreviousHierarchy(Mite self, ParsingState state, boolean up) { showPrev(self, state, null, up) }

  boolean isRouteWrong(Mite self, ParsingState state) { wrongRoute.call(self, state) }

  boolean isMiteSatisfied(Mite self) { this.isSatisfied.call(self) }

  Construction withAction(Function2<ParsingState, Map, ParsingState> action) {
    this.action = action
    return this
  }
  Construction enrichingMites(Function3<Mite, List<Mite>, ParsingState, Collection<Mite>> action) {
    enrichAction = action
    return this
  }

  Construction structural(Function4<Mite, ParsingState, Mite, Boolean, List<ParsingState>> action) {
    showPrev = action
    return this
  }

  Construction routeWrong(Function2<Mite, ParsingState, Boolean> condition) {
    wrongRoute = condition
    return this
  }

  Construction satisfiedWhen(Predicate1<Mite> condition) {
    isSatisfied = condition
    return this
  }

  Mite call(Map miteContents) {
    return new Mite(miteContents, this)
  }

  @Override
  String toString() {
    return name
  }

  boolean isExecutable(Mite mite) {
    if (!requiredAttrs) return true

    for (attr in requiredAttrs) {
      if (!mite.contents[attr]) {
        return false
      }
    }
    return true
  }

  static Construction cxt(String name, List<String> requiredAttrs = [], Function2<ParsingState, Map, ParsingState> cl) {
    new Construction(name, requiredAttrs).withAction(cl)
  }
  static Construction cxt(String name, List<String> requiredAttrs = [], Closure cl) {
    new Construction(name, requiredAttrs).withAction(cl as Function2)
  }

}
