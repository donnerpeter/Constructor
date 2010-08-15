package cons2

import cons2.Log.AspectEvent
import cons2.Log.FrameEvent

/**
 * @author peter
 */
class Frame {
  Chart chart
  private final Map<String, List<Aspect>> alternatives = [:]
  private final Map<String, Aspect> chosen = [:]
  private final Set<Aspect> initialized = []
  Frame adopter
  Frame[] children
  int thematic

  def Frame(Chart chart, int thematic, Frame... args) {
    this.chart = chart
    this.thematic = thematic
    this.children = args
    chart.event(new FrameEvent(this, false))
  }

  void removeAlternative(Aspect construct) {
    def role = construct.role
    alternatives[role].remove construct
    setChosen(role,  alternatives[role].size() != 1 ? null : construct)
  }

  Frame enrich(Aspect... constructs) {
    addConstructs(constructs)
  }

  Frame addConstructs(Aspect... aspects) {
    Set<String> affectedRoles = []
    List<Aspect> added = []
    aspects.each { aspect ->
      def role = aspect.role
      def existing = alternatives.get(role, [])
      if (aspect in existing && chosen[role] == aspect) {
        return
      }

      affectedRoles << role
      existing << aspect
      added << aspect
    }
    affectedRoles.each { role ->
      setChosen(role,  alternatives[role].size() != 1 ? null : alternatives[role][0])
    }
    added.each {
      if (!chosen[it.role] || chosen[it.role] == it) {
        chart.aspectAppeared this, it
      }
      initialized << it
    }
    this
  }

  List<Frame> strongUsages(pattern) {
    chart.usages(this, false, pattern)
  }

  Frame findAround(boolean thematic, def pattern) {
    return findAllActiveAround(thematic, pattern).find { true }
  }

  List<Frame> findAllActiveAround(boolean thematic, def pattern) {
    def active = chart.findActive(thematic, pattern)
    return active - this
  }

  List<Frame> getThematicChildren() {
    thematic >= 0 ? [children[thematic]] : []
  }

  void chosenAs(def pattern) {
    def found = findDimension(pattern)
    if (found) {
      setChosen(found.role, found)
    }
  }

  private def setChosen(String role, Aspect cxt) {
    if (chosen[role] == cxt) return

    chosen[role] = cxt
    chart.event(new AspectEvent(this, role, cxt))
  }

  private Aspect findDimension(pattern) {
    for (role in roles) {
      def found = strongAlternatives(role).find { it.ping(pattern) }
      if (found) return found
    }
    return null
  }

  Aspect getAt(String role) {
    return chosen[role] 
  }

  private Collection<String> getRoles() { alternatives.keySet() }

  private List<Aspect> strongAlternatives(String role) {
    return chosen[role] ? [chosen[role]] : alternatives[role]
  }

  boolean ping(boolean thematic, def pattern) {
    if (thematic && strongUsages([]).any { ((Frame) this) in it.thematicChildren }) {
      return false
    }

    return findDimension(pattern) != null
  }

  @Override
  String toString() {
    "[" + chosenAspects.collect { it.role + ":" + it }.join(", ") + "]"
  }

  List<Aspect> getChosenAspects() {
    roles.findAll { chosen[it] }.collect { chosen[it] }
  }

  List<Aspect> getInitializedAspects() {
    chosenAspects.findAll { it in initialized }
  }

  void relax() {
    chosen.clear()
    chart.activate this
  }

  void adoptContent(Frame source) {
    source.alternatives.each { role, constructs ->
      def wasEmpty = !alternatives[role]
      alternatives.get(role, []).addAll(constructs)
      if (wasEmpty) {
        setChosen role, source.chosen[role]
      }
    }
    source.adopter = this
    chart.weaken source
  }
}

