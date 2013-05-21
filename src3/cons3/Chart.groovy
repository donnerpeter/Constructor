package cons3

import groovy.transform.Canonical
import groovy.transform.EqualsAndHashCode
import groovy.transform.TupleConstructor
import org.pcollections.Empty
import org.pcollections.PCollection
import org.pcollections.PSequence

/**
 * @author peter
 */
@EqualsAndHashCode
class Chart {
  private final PSequence<Object> currentMetas
  final IdentityHashMap<Assignment<Variable>, PSequence<Object>> __metas
  private final FLinkedMap<Object, List<Set<Variable>>> metaUnifications
  final int section
  final LinkedHashMap<Key, Unit> allUnits
  final Set<Key> activeKeys
  final Key currentKey

  @TupleConstructor
  private static class Unit {
    final List<Assignment<Variable>> assignments
    final List<Pair<Variable, Variable>> unifications

    @Override
    public String toString() {
      return "Unit{" + "assignments=" + assignments + ", unifications=" + unifications + '}';
    }

  }

  @Canonical
  private static class Key {
    final Object object

    @Override
    String toString() {
      return object.toString()
    }

  }

  private Chart(Map args, Chart base) {
    currentMetas = getOrElse(args, 'currentMetas', base.currentMetas)
    __metas = getOrElse(args, 'metas', base.__metas)
    metaUnifications = getOrElse(args, 'metaUnifications', base.metaUnifications)
    section = getOrElse(args, 'section', base.section)
    allUnits = getOrElse(args, 'allUnits', base.allUnits)
    activeKeys = getOrElse(args, 'activeKeys', base.activeKeys)
    currentKey = getOrElse(args, 'currentKey', base.currentKey)

    assert allUnits.containsKey(currentKey)
    if (allUnits.size() < activeKeys.size()) {
      assert false
    }
  }

  private static <T> T getOrElse(Map m, String key, T defaultValue) {
    def result = (T) m.containsKey(key) ? m[key] : defaultValue
    assert result != null
    return result
  }

  Chart() {
    currentMetas = Empty.stack()
    __metas = new IdentityHashMap()
    metaUnifications = FLinkedMap.emptyMap
    section = 1

    def key = new Key(new Object())
    currentKey = key
    allUnits = [(key):new Unit([], [])]
    activeKeys = [key]
  }

  Chart nextUnit(Object id, List<Pair<Variable, Variable>> unifications, boolean activate) {
    def key = new Key(id)
    def newActive = activate ? activeKeys + [key] : activeKeys
    def newAll = allUnits + [(key):new Unit([], unifications)]

    def oldUnit = allUnits[currentKey]
    if (!oldUnit.assignments && !oldUnit.unifications && currentKey.object.class == Object) {
      newActive = new HashSet(newActive)
      newActive.remove(currentKey)
      newAll.remove(currentKey)
    }
    clone(currentKey:key, allUnits:newAll, activeKeys:newActive)
  }

  Chart changeActiveUnits(Collection deactivated, Collection activated) {
    Set<Key> minusKeys = deactivated.collect { new Key(it) } as Set
    Set<Key> plusKeys = activated.collect { new Key(it) } as Set
    clone(activeKeys:Util.minus(activeKeys, minusKeys) + plusKeys)
  }

  Chart clone(Map update) {
    return new Chart(update, this)
  }

  List<Pair<Variable, Variable>> getActiveUnifications() {
    def result = []
    for (key in allUnits.keySet()) {
      if (key in activeKeys) {
        result.addAll(allUnits[key].unifications)
      }
    }
    result
  }

  private List<Assignment<Variable>> cachedActiveAssignments
  List<Assignment<Variable>> getActiveAssignments() {
    if (cachedActiveAssignments == null) {
      LinkedHashSet<Assignment<Variable>> result = []
      for (key in allUnits.keySet()) {
        if (key in activeKeys) {
          result.addAll(allUnits[key].assignments)
        }
      }
      cachedActiveAssignments = result as List
    }
    return cachedActiveAssignments
  }

  private List<Frame> cachedFrames

  List<Frame> getFrames() {
    if (cachedFrames == null) {
      cachedFrames = new ArrayList<Frame>(new LinkedHashSet<Frame>(vars.collect { it.frame(this) }))
    }
    return cachedFrames
  }


  boolean earlier(Frame f1, String a1, Frame f2, String a2) {
    List<Assignment<Frame>> assignments = allAssignments
    Assignment<Frame> i1 = assignments.find { Assignment it -> it.frame == f1 && it.property == a1 }
    Assignment<Frame> i2 = assignments.find { Assignment it -> it.frame == f2 && it.property == a2 }
    if (!i1 || !i2) return false
    return i1.generation < i2.generation
  }

  List<Variable> getVars() {
    Set<Variable> set = [] as LinkedHashSet
    for (assignment in activeAssignments) {
      set << assignment.frame
      if (assignment.value instanceof Variable) {
        set << (Variable) assignment.value
      }
    }
    return set as List
  }

  private List<Assignment<Frame>> assignmentCache

  List<Assignment<Frame>> getAllAssignments() {
    def result = assignmentCache
    if (result != null) {
      return result
    }

    result = activeAssignments.collect { framify(it) }
    assignmentCache = result
    return result
  }

  Assignment<Frame> framify(Assignment<Variable> it) {
    def oldValue = it.value
    def newValue = oldValue instanceof String ? oldValue : ((Variable) (Object) oldValue).frame(this)
    Frame newFrame = it.frame.frame(this)
    return new Assignment<Frame>(newFrame, it.property, newValue, it.generation, it.section)
  }

  Chart advanceSection() {
    clone(section:section+1)
  }

  Chart assign(Variable var, String property, def value, int generation) {
    assert value instanceof String || value instanceof Variable
    assert var

    def assignment = new Assignment(var, property, value, generation, section)
    def newMetas = new IdentityHashMap<Assignment<Variable>, PSequence<Object>>(__metas)
    newMetas[assignment] = currentMetas

    Unit oldUnit = allUnits[currentKey]
    List<Assignment<Variable>> newUnitAss = oldUnit.assignments + assignment
    def newUnit = new Unit(newUnitAss, oldUnit.unifications)

    return clone(metas:newMetas, allUnits:allUnits + [(currentKey):newUnit])
  }

  private Function2<Variable, Integer, String> createNamer() {
    Map<Variable, String> shortNames = [:]
    Map<Variable, Integer> sections = [:]
    Map<Integer, Character> nextShortName = [:]
    return { Variable var, Integer currentSection ->
      var = getUnifiedVar(var)
      if (!(var in shortNames)) {
        def ch = nextShortName.get(currentSection, 'A' as Character)
        shortNames[var] = String.valueOf(ch)
        nextShortName[currentSection] = (ch + 1) as Character
        sections[var] = currentSection
      }
      if (currentSection == sections[var]) return shortNames[var]
      return shortNames[var] + "@" + sections[var]
    } as Function2
  }

  String presentable() {

    Function2<Variable, Integer, String> namer = createNamer()

    Set<Assignment<Frame>> handled = []
    Set<Variable> prevMetas = []
    List<String> lines = []
    int curSection = 1

    for (eq in activeAssignments) {
      if (curSection != eq.section) {
        lines << "-- ${eq.section}:"
        curSection = eq.section
      }
      if (!handled.add(framify(eq))) {
        continue
      }

      Set<Variable> metas = __metas[eq].findAll { it instanceof Variable } as Set
      for (finished in (prevMetas - metas)) {
        lines << "</${namer(finished, curSection)}>"
      }
      for (started in (metas - prevMetas)) {
        lines << "<${namer(started, curSection)}>"
      }
      prevMetas = metas
      lines << "${namer(eq.frame, curSection)}.$eq.property=${ eq.value instanceof String ? eq.value : namer((Variable)eq.value, curSection)}"
    }
    for (finished in prevMetas) {
      lines << "</${namer(finished, curSection)}>"
    }
    return lines.join("\n")
  }

  Chart startMeta(meta) {
    return clone(currentMetas: currentMetas + meta)
  }

  Chart finishMeta(meta) {
    return clone(currentMetas: currentMetas - meta)
  }

  private Map<Variable, Variable> __baseVars
  Variable getUnifiedVar(Variable var) {
    if (__baseVars == null) {
      __baseVars = [:]
      Map<Variable, Set<Variable>> groups = [:]

      for (set in activeUnifications) {
        def v1 = set.first.base
        def v2 = set.second.base
        Set<Variable> group = groups[v1] ?: new LinkedHashSet<Variable>([v1])
        group.addAll(groups[v2] ?: new LinkedHashSet<Variable>([v2]))
        group << v1
        group << v2
        for (v in group) {
          groups[v] = group
        }
      }

      for (set in groups.values()) {
        Variable main = set.iterator().next()
        for (v in set) {
          __baseVars[v] = main
        }

      }
    }
    return __baseVars[var.base] ?: var.base
  }

}