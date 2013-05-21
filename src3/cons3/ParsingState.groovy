package cons3

import org.pcollections.ConsPStack
import org.pcollections.PStack

/**
 * @author peter
 */
class ParsingState {
  static final ParsingState EMPTY = new ParsingState()
  final Chart chart
  final List<Mite> contribution
  final List<Mite> currentContribution
  final Network network
  final int currentGeneration
  private Map<Construction, List<Mite>> vmCache
  final List<Mite> ownMites
  final ParsingState prevState
  private final List log

  ParsingState() {
    this([chart: new Chart(), prevState:EMPTY, network:new Network([:], [:], [:], [] as LinkedHashSet)], null)
  }

  private ParsingState(Map map, ParsingState base) {
    chart = map.chart ?: base.chart
    currentGeneration = map.currentGeneration ?: base?.currentGeneration ?: 0
    network = map.network ?: base.network
    contribution = map.contribution ?: base?.contribution ?: []
    currentContribution = map.currentContribution ?: []
    ownMites = map.ownMites ?: base?.ownMites ?: []
    prevState = map.prevState ?: base?.prevState
    log = map.log ?: base?.log ?: []
  }

  List<Mite> getMiteList() {
    return visibleMites.values().collect { it.reverse() }.flatten() as List
  }

  Map<Construction, List<Mite>> getVisibleMites() {
    if (vmCache == null) {
      vmCache = this.calcVisibleMites()
    }
    return vmCache
  }

  private ParsingState clone(Map update) {
    return new ParsingState(update, this)
  }


  ParsingState assign(def var, String property, def value) {
    clone(chart: chart.assign(((Variable)var).base, property, value instanceof Variable ? value.base : value, currentGeneration))
  }

  ParsingState advanceSection() {
    clone(chart: chart.advanceSection())
  }

  Map getAt(Construction c) {
    def allMites = visibleMites[c]
    if (allMites == null) {
      return null
    }

    for (Mite mite in allMites.reverse()) {
      if (network.isChosen(mite)) {
        return mite.contents
      }
    }

    Map value = [:]
    for (part in allMites.reverse()) {
      if (!Mite.overwrites(value, part.contents)) {
        value = Mite.unify(value, part.contents)
      }
    }
    value
  }

  @Override
  String toString() {
    return this == EMPTY ? "##" : "#$currentGeneration"
  }

  ParsingState startMeta(Variable meta) {
    return clone(chart: chart.startMeta(meta))
  }
  ParsingState finishMeta(Variable meta) {
    return clone(chart: chart.finishMeta(meta))
  }

  private Mite nextInterceptor(Construction start) {
    def mites = visibleMites
    List<Construction> cs = mites.keySet() as List
    def index = cs.indexOf(start)
    if (index >= 0 || !start) {
      def tail = cs.subList(index + 1, cs.size())
      for (cxt in tail) {
        def mite = mites[cxt].reverse().find { it.contents.interceptor }
        if (mite) {
          return mite
        }
      }
    }
    return null
  }

  ParsingState apply(Map<Construction, Map> constructions) {
    apply((constructions.keySet() as List<Construction>).collect { it(constructions[it]) } as List)
  }
  ParsingState apply(Mite... constructions) { apply(constructions as List) }
  ParsingState apply(List<Mite> constructions) {
    if (!constructions) return this

    def interceptor = nextInterceptor(null)
    if (interceptor) {
      return ((Interceptor) interceptor.contents.interceptor).intercept(constructions, this, { c, s -> baseApply(s, c, true) } as Function2)
    }
    return baseApply(this, constructions, true)
  }

  private static ParsingState baseApply(ParsingState state, List<Mite> update, boolean mayCorrectRoute) {
    LinkedHashSet<Mite> fullUpdate = update as LinkedHashSet<Mite>
    fullUpdate.addAll(state.enrichUpdate(update, state.clone(currentContribution:update)))
    assert fullUpdate

    state = state.clone(ownMites:fullUpdate as List, prevState:state,
            network:state.network.addMites(fullUpdate),
            contribution:update, currentGeneration:state.currentGeneration + 1)
    state = applyConstructions(fullUpdate, state)
    state = state.clone(chart:state.chart.nextUnit(new Object(), [], true))

    def newActive = state.network.choose(fullUpdate as List)
    def sat = newActive.maximizeSatisfiedness([])
    state = activate(state, sat)

    state = processAlternatives(state, fullUpdate)

    state = state.appendLog(state)

    if (mayCorrectRoute) {
      def traitor = state.findTraitor()
      if (traitor) {
        state = correctRoute(state.prevState, ConsPStack.from([state]), state.log + ["changing route because of $traitor\n"] as List) ?: state
      }
    }

    return state
  }

  ParsingState appendLog(entry) {
    clone(log:log + entry)
  }

  private static ParsingState processAlternatives(ParsingState state, LinkedHashSet<Mite> fullUpdate) {
    Set<Set<Mite>> tried = [state.network.chosenExecutable]
    for (cxt in state.visibleMites.keySet()) {
      if (!(cxt in state.visibleMites)) continue

      List<Mite> toCheck = (fullUpdate as List<Mite>).findAll { it.cxt == cxt }
      def last = state.visibleMites[cxt].reverse()[0]
      if (last && !(last in toCheck)) {
        toCheck << last
      }

      for (toAdd in toCheck.reverse()) {
        if (!state.network.isChosen(toAdd)) {
          state = tryAlternative(state, toAdd, tried)
        }
      }
    }
    return state
  }

  private static ParsingState correctRoute(ParsingState state, PStack<ParsingState> reapply, List log) {
    for (mite in state.ownMites) {
      if (!state.network.isChosen(mite)) {
        def anotherActive = state.network.choose([mite]).maximizeSatisfiedness([mite])
        def alternative = activate(state.clone(log:log), anotherActive)
        for (step in reapply) {
          alternative = baseApply(alternative, step.contribution, false)
        }
        if (!alternative.findTraitor()) {
          return alternative
        }
      }
    }
    if (state.prevState) {
      return correctRoute(state.prevState, reapply + state, log)
    }
    return null
  }

  private Mite findTraitor() {
    miteList.find { it.cxt.isRouteWrong(it, this) }
  }

  ParsingState getPrevVisibleState() {
    def h = hierarchy
    return h.size() == 1 ? null : h[1]
  }

  boolean isLinkUp() {
    return doFindLink(true) != null
  }

  private Mite findLink() {
    return doFindLink(true) ?: doFindLink(false)
  }
  private Mite doFindLink(boolean up) {
    ParsingState bestPrev = null
    Mite bestLink = null
    for (mite in ownMites) {
      if (!network.isChosen(mite)) continue

      def history = mite.cxt.showPreviousHierarchy(mite, prevState, up)
      if (!history) continue

      ParsingState somePrev = history[0]
      if (somePrev == EMPTY) {
        return mite
      }
      if (somePrev && (bestPrev == null || bestPrev.isMoreRecentThan(somePrev))) {
        bestPrev = somePrev
        bestLink = mite
      }
    }
    return bestLink
  }

  private LinkedHashSet<Mite> findAllLinks() {
    def link = findLink()
    if (!link) return []

    def prev = getPrevHierarchyFromLink(link)
    assert prev
    return ownMites.findAll { prev == getPrevHierarchyFromLink(it) } as LinkedHashSet
  }

  boolean isMoreRecentThan(ParsingState state) {
    return state == this || prevState?.isMoreRecentThan(state)
  }

  LinkedHashSet<Mite> enrichUpdate(List<Mite> update, ParsingState original) {
    LinkedHashSet<Mite> fullUpdate = []
    for (state in hierarchy) {
      for (mite in state.findMitesAlongParsingRoute(original)) {
        fullUpdate.addAll(Util.reverse(mite.cxt.enrichUpdate(mite, update, original)))
      }
    }
    fullUpdate
  }

  private List<ParsingState> cachedHierarchy
  List<ParsingState> getHierarchy() {
    if (cachedHierarchy != null) {
      return cachedHierarchy
    }

    List<ParsingState> result = [this]
    Mite link = findLink()
    def prev = getPrevHierarchyFromLink(link) ?: prevState?.hierarchy
    if (prev) {
      result.addAll(prev)
    }
    return cachedHierarchy = result
  }

  private List<Mite> findMitesAlongParsingRoute(ParsingState original) {
    LinkedHashSet<Mite> links = findAllLinks()
    return ownMites.findAll { mite -> !links || links.find { !original.network.contradict(mite, it) } }
  }

  List<ParsingState> getPrevHierarchyFromLink(Mite link) {
    return link?.cxt?.showPreviousHierarchy(link, prevState, true) ?: link?.cxt?.showPreviousHierarchy(link, prevState, false)
  }

  private static ParsingState tryAlternative(ParsingState state, Mite preferred, Set<Set<Mite>> tried) {
    def anotherActive = state.network.choose([preferred]).maximizeSatisfiedness([preferred])
    if (!tried.add(anotherActive.chosenExecutable)) return state

    return doTryAlternative(state, anotherActive)
  }

  private static ParsingState doTryAlternative(ParsingState state, Network anotherActive) {
    def alternative = activate(state, anotherActive)
    try {
      if (new Domination(alternative, state).dominatesSemantically()) {
        return alternative
      }
    } catch (Throwable e) {
      println alternative.chart.presentable() + "\n\n"
      println alternative.logString + "\n\n"
      println alternative.presentable() + "\n\n"
      throw e
    }
    return state
  }

  private static ParsingState activate(ParsingState state, Network newActive) {
    def oldActive = state.network
    def toRemove = Util.minus(oldActive._chosen, newActive._chosen)
    LinkedHashSet<Mite> toApply = Util.minus(newActive._chosen, oldActive._chosen)

    return state.clone(network:newActive, chart:state.chart.changeActiveUnits(toRemove, toApply))
  }

  private static ParsingState applyConstructions(LinkedHashSet<Mite> toApply, ParsingState state) {
    for (mite in toApply) {
      state = state.clone(chart:state.chart.nextUnit(mite, mite.unifications, false))
      if (mite.executable) {
        state = mite.cxt.applySemantics(mite, state)
      }
    }
    return state
  }

  String presentable() {
    def log = ""
    def mites = visibleMites
    def links = findAllLinks()
    def prevHierarchy = hierarchy[1..<hierarchy.size()-1]
    log += "$this->${prevHierarchy.join(",")},  uni: " + chart.activeUnifications.collect { "$it.first=$it.second" }.join(", ") + "\n"
    for (k in mites.keySet()) {
      log += "    $k: " + mites[k].collect {
        (it in links ? '!!' : '') + (network.isChosen(it) ? '*' : '') + (it.satisfied ? '' : '@') +
        new LinkedHashMap(it.contents.reverse()) +
        (it.unifications ? ';' + it.unifications.collect { "<$it.first=$it.second>" }.join("") : '')
      }.join(" ") +"\n"
    }
    LinkedHashSet<Mite> invisible = network.chosenUnsatisfied.findAll { !(it in miteList) }
    if (invisible) {
      log += "    invisible: " + invisible.collect { "${network.isChosen(it) ? '*' : ''}@$it" }.join(", ") + "\n"
    }
    return log + "\n"
  }

  ParsingState findState(Mite mite, String attr) {
    history.reverse().find { it.ownMites.find { it.contents[attr] && it.isPartOf(mite) } }
  }
  ParsingState findState(Mite mite) {
    if (mite in ownMites) {
      return this
    }
    return prevState?.findState(mite)
  }

  List<ParsingState> getHistory() {
    def result = []
    def each = this
    while (each) {
      result << each
      each = each.prevState
    }
    result
  }

  private Map<Construction, List<Mite>> calcVisibleMites() {
    Map<Construction, List<Mite>> result = [:]
    for (state in hierarchy) {
      state.ownMites.groupBy { it.cxt }.each { cxt, group ->
        List updated = new ArrayList(group)
        if (result[cxt]) {
          updated.addAll(result[cxt])
        }
        result[cxt] = updated
      }
    }
    result
  }

  String getLogString() {
    log.collect { it instanceof ParsingState ? it.presentable() : it.toString() }.join('')
  }

  boolean isEmpty() {
    return prevState == null
  }

}