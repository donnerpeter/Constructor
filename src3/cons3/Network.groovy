package cons3

import groovy.transform.CompileStatic
import groovy.transform.TupleConstructor
/**
 * @author peter
 */
@TupleConstructor(includeFields = true)
@CompileStatic
class Network {
  final Map<Mite, Collection<Mite>> contradictors = [:]
  final Map<Object, Collection<Mite>> groups = [:]
  final Map<Mite, Collection<ExternalContradiction>> externalGroups = [:]
  final LinkedHashSet<Mite> _chosen = []
  private final Map<Mite, LinkedHashSet<Object>> groupCache = [:]

  Network maximizeSatisfiedness(List<Mite> fixed) {
    Network net = this
    while (true) {
      Network better = tryMaximizeSatisfiedness(net, fixed)
      if (better == net) {
        return net
      }
      net = better
    }
  }
  private static Network tryMaximizeSatisfiedness(Network net, List<Mite> fixed) {
    Network best = net
    for (mite in net.chosenUnsatisfied) {
      for (alt in net.contradictors[mite]) {
        if (!alt.satisfied || fixed.find { Mite eachFixed -> net.contradict(eachFixed, alt) }) continue

        def appended = (fixed + [alt]) as List<Mite>

        def alternative = net.choose(appended)

        def unsatisfied1 = best.chosenUnsatisfied
        def unsatisfied2 = alternative.chosenUnsatisfied
        if (unsatisfied2.size() <= unsatisfied1.size()) {
          alternative = tryMaximizeSatisfiedness(alternative, appended)
          unsatisfied2 = alternative.chosenUnsatisfied
          if (unsatisfied2.size() < unsatisfied1.size()) {
            best = alternative
          }
        }
      }
    }
    return best
  }

  Network addMites(LinkedHashSet<Mite> mites) {
    def withGroups = updateGroups(mites)
    def result = withGroups.updateContradictors(mites)
    return result
  }

  private Network updateGroups(LinkedHashSet<Mite> mites) {
    Map<Object, Collection<Mite>> newGroups = [:] + groups
    Map<Mite, Collection<ExternalContradiction>> newExternal = [:] + externalGroups

    for (mite in mites) {
      for (id in calcInternalGroups(mite)) {
        if (id instanceof ExternalContradiction) {
          List<Mite> members = ((ExternalContradiction) id).contradictsTo + mite as List
          for (Mite contr in members) {
            newExternal[contr] = (newExternal[contr] ?: []) + [id] as List
          }
          newGroups[id] = (newGroups[id] ?: []) + members as List
        } else {
          newGroups[id] = (newGroups[id] ?: []) + [mite] as List
        }
      }
    }

    return new Network(contradictors, newGroups, newExternal, _chosen)
  }

  private Network updateContradictors(LinkedHashSet<Mite> mites) {
    Map<Mite, Collection<Mite>> newContradictors = [:] + contradictors
    for (mite in mites) {
      LinkedHashSet<Mite> contras = findContradictors(mite)
      newContradictors[mite] = contras
      for (c in contras) {
        if (!(c in mites)) {
          newContradictors[c] = newContradictors[c] + mite
        }
      }
    }
    return new Network(newContradictors, groups, externalGroups, _chosen)
  }

  private LinkedHashSet<Mite> findContradictors(Mite mite) {
    LinkedHashSet<Mite> contras = []
    for (id in allGroups(mite)) {
      contras.addAll(groups[id])
    }
    contras.remove(mite)
    return contras
  }

  boolean isChosen(Mite mite) {
    mite in _chosen
  }

  boolean contradict(Mite m1, Mite m2) {
    m1 != m2 && Util.intersects(allGroups(m1), allGroups(m2))
  }

  private static Mite findChosenAncestor(Mite mite, Set<Mite> chosen) {
    while (mite) {
      if (mite in chosen) return mite
      mite = mite.src1
    }
    return null
  }

  private LinkedHashSet<Object> allGroups(Mite mite) {
    LinkedHashSet<Object> result = groupCache[mite]
    if (result != null) return result

    result = calcInternalGroups(mite)
    def external = externalGroups[mite]
    if (external) {
      result.addAll(external)
    }
    groupCache[mite] = result
    return result
  }

  private static LinkedHashSet<Object> calcInternalGroups(Mite mite) {
    LinkedHashSet<Object> result
    result = new LinkedHashSet<Object>(mite.primaries)
    Set xor = (Set) mite.contents.xor
    if (xor) {
      result += xor
    }
    return result
  }

  private Mite findMiteWithMostGroups(Collection<Mite> mites, LinkedHashSet<Object> queue) {
    Mite best = null
    for (mite in mites) {
      def groups = allGroups(mite)
      if (best == null || Util.intersect(groups, queue).size() > Util.intersect(allGroups(best), queue).size()) {
        best = mite
      }
    }
    return best
  }

  private Mite findBest(LinkedHashSet<Mite> unprocessed, LinkedHashSet<Mite> chosen, LinkedHashSet<Object> queue) {
    def best
    if (!best) {
      best = findMiteWithMostGroups(unprocessed.findAll { Mite mite -> !mite.atom && mite.satisfied }, queue)
    }
    if (!best) {
      best = findMiteWithMostGroups(unprocessed.findAll { Mite mite -> !mite.atom && contradictors[mite].every { !(it in chosen) } }, queue)
    }
    if (!best) {
      best = findMiteWithMostGroups(unprocessed.findAll { Mite mite -> !mite.atom }, queue)
    }
    if (!best) {
      best = findMiteWithMostGroups(unprocessed, queue)
    }
    return best
  }

  private LinkedHashSet<Mite> filterNonContradicting(LinkedHashSet<Mite> updated) {
    LinkedHashSet<Mite> filtered = []
    for (mite in updated) {
      if (!Util.intersects(contradictors[mite], filtered)) {
        filtered << mite
      }
    }
    return filtered
  }
  private LinkedHashSet<Mite> rearrangeUpdatedMites(List<Mite> updated) {
    LinkedHashSet<Mite> preferred = []
    preferred.addAll(updated.findAll { Mite mite -> findChosenAncestor(mite, _chosen) })
    preferred.addAll(updated.findAll { Mite mite -> !mite.atom && !hasChosenMergedContradictors(mite) })
    preferred.addAll(updated)
    return preferred
  }

  private boolean hasChosenMergedContradictors(Mite mite) {
    return contradictors[mite].any { Mite m -> !m.atom && isChosen(m) }
  }


  Network choose(Collection<Mite> preferred) {
    LinkedHashSet<Mite> result = new LinkedHashSet<Mite>(_chosen)

    LinkedHashSet<Object> groupsToProcess = []
    LinkedHashSet<Object> processed = []

    for (mite in filterNonContradicting(rearrangeUpdatedMites(new ArrayList<Mite>(preferred)))) {
      result << mite
      processed.addAll(allGroups(mite))
      for (displaced in Util.intersect(contradictors[mite], result)) {
        result.remove(displaced)
        groupsToProcess.addAll(allGroups(displaced))
      }
    }
    groupsToProcess.removeAll(processed)

    while (groupsToProcess) {
      LinkedHashSet<Mite> unprocessed = []
      for (group in groupsToProcess) {
        for (elem in groups[group]) {
          if (!(elem in unprocessed) && !Util.intersects(allGroups(elem), processed)) {
            unprocessed << elem
          }
        }
      }

      Mite best = findBest(unprocessed, result, groupsToProcess)
      if (!best) {
        break
      }

      for (victim in Util.intersect(contradictors[best], result)) {
        result.remove(victim)
        def more = allGroups(victim)
        assert !Util.intersects(more, processed)
        groupsToProcess = (LinkedHashSet<Object>) new LinkedHashSet<Object>(more) + groupsToProcess
      }
      result << best
      groupsToProcess.removeAll(allGroups(best))
      processed.addAll(allGroups(best))
    }

    new Network(contradictors, groups, externalGroups, result, groupCache)
  }

  LinkedHashSet<Mite> getChosenUnsatisfied() { return _chosen.findAll { Mite mite -> !mite.satisfied } }

  LinkedHashSet<Mite> getChosenExecutable() { return _chosen.findAll { Mite mite -> mite.executable } }


}

@TupleConstructor
class ExternalContradiction {
  final String stringId
  final List<Mite> contradictsTo

  @Override
  String toString() {
    return "external[$stringId]"
  }
}