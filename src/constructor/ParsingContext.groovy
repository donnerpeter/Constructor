package constructor

/**
 * @author peter
 */
class ParsingContext {
  private Construction relativeTo
  Cloud cloud

  def ParsingContext(Construction relativeTo, Cloud cloud) {
    this.relativeTo = relativeTo
    this.cloud = cloud
  }

  Construction findBefore(Construction anchor, hint, serious = true) {
    def result = cloud.match(relativeTo, [hint, '_'], serious, noUsedArgs())
    return result ? result[0] : null
  }

  private IntRange compositeRange(Construction newC) {
    if (newC.descr == PhraseConstruction.HEAD) {
      return cloud.ranges[newC.args[1]]
    }

    int _min = Integer.MAX_VALUE
    int _max = Integer.MIN_VALUE
    newC.args.each {arg ->
      _min = Math.min(_min, cloud.startOffset(arg))
      _max = Math.max(_max, cloud.endOffset(arg))
    }
    return _min.._max
  }

  def addConstruction(Construction c) {
    cloud.weak.remove c
    if (cloud.ranges.containsKey(c)) return

    cloud.addConstructions([c], compositeRange(c))
  }

  Collection<Construction> allUsages(Construction c, hint) {
    return cloud.allUsages(PhraseConstruction.project(c, cloud), hint)
  }

  Collection<Construction> strongUsages(Construction c, hint) {
    return cloud.strongUsages(PhraseConstruction.project(c, cloud), hint)
  }

  Construction coloredBetween(Construction from, Construction to) {
    cloud.coloredRange(cloud.endOffset(from)..cloud.startOffset(to))
  }

  Construction near(Construction c, boolean after, hint) {
    return cloud.allAt(after ? cloud.endOffset(c) : cloud.startOffset(c), after).find { it.isAccepted(hint, cloud) }
  }

  private Function1<Construction, Boolean> noUsedArgs() {
    Set<Construction> usedArgs = [] as Set
    strongUsages(relativeTo, []).each { Construction usg ->
      if (usg.descr._transparent) {
        return
      }

      (usg.args - relativeTo).each {
        usedArgs += cloud.competitors[it]
      }
    }

    return { !(it in usedArgs) } as Function1
  }

  List<Construction> match(List pattern) {
    return cloud.match(relativeTo, pattern, noUsedArgs())
  }

  def markFinished(Construction construction) {
    cloud.markFinished construction
  }

  def relaxUsages(hint) {
    strongUsages(relativeTo, hint).each { usg ->
      usg.args.each {
        cloud.competitors[it].each {
          cloud.weak << usg
          cloud.weak -= it
          cloud.promote it
          if (it instanceof PhraseConstruction) {
            cloud.weak -= cloud.allUsages(it, PhraseConstruction.HEAD.name)
          }
        }
      }
    }
  }

  def weaken(Construction c) {
    cloud.weak << c
    demote(c)
  }

  def demote(Construction c) {
    cloud.demote(c)
  }
}

