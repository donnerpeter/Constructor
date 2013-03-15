package cons3

import groovy.transform.TupleConstructor

import static cons3.RussianConstructions.*

/**
 * @author peter
 */
class SeqConstruction extends Construction {
  SeqConstruction() {
    super('seq')
  }

  @Override
  ParsingState applySemantics(Mite mite, ParsingState state) {
    def args = mite.contents
    String conj = args.conj
    if (args.second) {
      if (args.first) {
        state = state.assign(args.multi, 'member', args.first)
      }
      if (conj && conj != ',') {
        state = state.assign(args.multi, 'conj', conj)
      }
      if (args.distinguish) {
        state = state.assign(args.second, 'distinguished_in', args.multi)
      }
      state = state.assign(args.multi, 'member', args.second)
    }
    return state
  }

  @Override
  Collection<Mite> enrichUpdate(Mite self, List<Mite> update, ParsingState state) {
    new SeqInterceptor(self, update, state).enrichUpdate()
  }

  @Override
  List<ParsingState> showPreviousHierarchy(Mite mite, ParsingState state, boolean up) {
    if (mite.contents.mergedWith) {
      def mergePointHierarchy = ((ParsingState) mite.contents.mergedWith).hierarchy
      return mergePointHierarchy[1..<mergePointHierarchy.size()]
    }
    if (mite.contents.conj && !mite.contents.first && !mite.lastAtom.contents.questionLevel) {
      return [ParsingState.EMPTY]
    }
    return null
  }

  @Override
  boolean isRouteWrong(Mite mite, ParsingState state) {
    if (mite.contents.mergedWith) {
      if (state.ownMites.find { it.cxt == nomSubject && it.atom && it.contents.head }) {
        def nomArg = state.miteList.find { it.cxt == nom && it.contents.noun == mite.contents.second }
        if (nomArg && !state.network.isChosen(nomArg)) {
          return true
        }
      }
      return false
    }
    if (mite.contents.conj == ',') {
      for (c in [adverbialPhrase, poDatLuck, accordingTo]) {
        if (state.ownMites.find { it.cxt == c && it.atom && it.contents.content } &&
            !state.miteList.find { it.cxt == c && it.contents.comma1 && it.contents.content }) {
          return true
        }
      }
      if (state.ownMites.find { it.cxt == nomSubject && it.atom && it.contents.head }) {
        def advp = state.miteList.find { it.cxt == adverbialPhrase && it.contents.comma2 && it.contents.content }
        if (advp && !state.network.isChosen(advp)) {
          return true
        }
      }
    } else {
      if (state.ownMites.find { it.cxt == nomSubject && it.atom && it.contents.head }) {
        def beforeMites = state.findState(mite.firstAtom).prevState?.ownMites
        ParsingState prev = beforeMites?.find { it.cxt == declOrQuestionComp && it.contents.comma2 }?.contents?.beforeComma1
        if (prev && prev.visibleMites[nomSubject]?.find { it.atom && it.contents.head }) {
          return true
        }
      }
    }
    return false
  }
}

@TupleConstructor
class SeqInterceptor {
  final ParsingState before
  final Mite self
  final List<Mite> constructions
  final ParsingState state

  SeqInterceptor(Mite mite, List<Mite> constructions, ParsingState state) {
    before = mite.contents.beforeComma ?: state.findState(mite.firstAtom).prevState
    self = mite
    this.constructions = constructions
    this.state = state
  }

  private boolean isButAfterComma() {
    return self.contents.conj == ',' && !self.contents.multi &&
           constructions.find { it.cxt == seq && it.contents.conj == 'but' && !it.contents.mergedWith }
  }

  Collection<Mite> enrichUpdate() {
    if (isButAfterComma()) {
      return seq(beforeComma:before, xor:self.contents.xor).unifyWherePossible(constructions, false, false)
    }

    if (self.contents.mergedWith) {
      return []
    }

    def xor = new Tokens().a
    Update result = new Update()
    def allSimilar = findSimilarContributions()
    for (similar in allSimilar) {
      def variant = mergeWithSimilarState(similar, xor)
      result = result.xor(variant as Mite[])
    }
    return Util.reverse(result.mites)
  }

  private List<Mite> mergeWithSimilarState(ParsingState similar, Object xor) {
    List<Mite> result = []

    Map<Pair<Variable, Variable>, Variable> seqs = [:]

    ParsingState mergePoint = null
    LinkedHashSet newXors = [xor]

    for (mite in constructions) {
      Mite oldMite = findSimilarOwnMite(mite, similar)
      if (oldMite) {
        def nextPoint = merge(oldMite, mite, result, seqs)
        if (nextPoint) {
          mergePoint = nextPoint
          newXors.addAll(calcSeqXors(oldMite, mite, mergePoint))
        } else if (mite.cxt == nomSubject) {
          def oldState = before.findState(oldMite)
          def prevSeq = findPrevSeq(oldState, oldMite, 'head', true)
          if (!canJoinSeq(prevSeq)) {
            mergePoint = oldState
            Variable first = prevSeq?.contents?.second ?: oldMite.contents.head
            def newMulti = obtainMergedVariable(first, (Variable) mite.contents.head, seqs, false)
            if (prevSeq) {
              Variable veryFirst = prevSeq.contents.first
              result << prevSeq.firstAtom.unify(seq(mergedWith:prevSeq.contents.mergedWith, multi:createMergedVar(veryFirst, newMulti), second:newMulti, first:veryFirst, xor:prevSeq.contents.xor))
            }
          } else {
            mergePoint = getFirstMemberState(prevSeq)
            Variable first = prevSeq.contents.multi
            obtainMergedVariable(first, (Variable) mite.contents.head, seqs, true)
          }
        }
      }
    }

    if (mergePoint != null) {
      result.addAll(unifyHeadsWithMergePoint(similar))
      result.addAll(mergePoint.prevState.enrichUpdate(result, state))
      seqs.each { pair, multi ->
        result.add(0, self.unify(seq(mergedWith:mergePoint, multi:multi, second:pair.second, first:(multi == pair.first ? null : pair.first), xor:newXors)))
      }
    }

    return result
  }

  private LinkedHashSet calcSeqXors(Mite oldMite, Mite newMite, ParsingState nextPoint) {
    LinkedHashSet newXors = []

    def oldMerge = before.findState(oldMite)?.ownMites?.find { it.cxt == seq && it.contents.mergedWith == nextPoint }
    if (!oldMerge) {
      newXors.addAll(oldMite.primaries)
    }
    newXors.addAll(newMite.primaries)

    Set xor = Mite.mergeXor(oldMite.contents, newMite.contents)
    if (xor) {
      for (id in xor) {
        List<Mite> members = constructions.findAll { it.contents.xor && id in (Set) it.contents.xor }
        def existing = state.network.groups[id]
        if (existing) {
          members.addAll(existing)
        }
        assert members
        newXors << new ExternalContradiction(id.toString(), members)
      }
    }

    return newXors
  }

  private List<ParsingState> findSimilarContributions() {
    List<ParsingState> states = before.hierarchy.findAll { state -> hasSimilarOwnMite(state) }
    if (!states) return []

    def closest = states[0]
    Mite numQ = closest.ownMites.find { it.cxt == numQuantifier && it.contents.num && ((Variable) it.contents.noun).hard }
    if (numQ) {
      def better = closest.prevVisibleState
      if (better in states) {
        return states[states.indexOf(better)..<states.size()]
      }
    }

    return states
  }

  private boolean hasSimilarOwnMite(ParsingState state) {
    def similar = constructions.findAll { findSimilarOwnMite(it, state) }
    if (similar) {
      return true
    }
    return false
  }

  private Mite findSimilarOwnMite(Mite mite, ParsingState inState) {
    def wh = constructions.find { it.cxt == filler } || inState.ownMites.find { it.cxt == filler }
    if (mite.cxt in [nom, acc, gen, genArg1] && wh) return null

    def hasCopula = constructions.find { it.cxt == copula } || inState.ownMites.find { it.cxt == copula }
    if (mite.cxt == nomSubject && hasCopula) return null

    if (mite.cxt in [nom, acc, gen, genArg1] && mite.contents.head) return null
    if (mite.cxt in [dat] && mite.contents.head && inState.ownMites.find { it.cxt == preposition }) return null
    if (mite.cxt in [sentenceHolder, Construction.noArg, verbalModifier, verbHolder, quotedName, preposition, poDat]) return null
    return inState.ownMites.reverse().find { it.atom && it.isSimilarTo(mite) }
  }

  private List<Mite> unifyHeadsWithMergePoint(ParsingState mergePoint) {
    List<Mite> result = []
    for (newMite in state.currentContribution) {
      def cxt = newMite.cxt
      def attr = cxt in [nom, acc, gen, dat, poDat] ? 'noun' : cxt == possessive ? 'possessor' : null
      Mite oldPure = mergePoint.ownMites.find { it.isSimilarTo(newMite) && it.atom && agree(newMite, it) }
      if (attr && newMite.contents[attr] && newMite.contents.head && newMite.atom && oldPure) {
        Mite unified = argSharing(cxt:cxt, var:newMite.contents[attr], xor:newMite.xor + [newMite] as LinkedHashSet)
        List<Mite> oldUnified = mergePoint.ownMites.findAll { it.cxt == argSharing && it.contents.cxt == cxt }
        if (!oldUnified) {
          oldUnified << argSharing(cxt:cxt, xor:oldPure.xor + [oldPure] as LinkedHashSet, var:oldPure.contents[attr])
        }
        for (oldMite in oldUnified) {
          result << oldMite.unify(argSharing(var:newMite.contents[attr]))
          for (atom in oldMite.primaries.findAll { it.contents.var } ) {
            unified = argSharing(var:atom.contents.var).unify(unified)
          }
        }
        result << unified
      }
    }
    return result
  }

  private static boolean agree(Mite mite, Mite oldMite) {
    return mite.contents.agrNumber == oldMite.contents.agrNumber
  }

  private static Mite findPrevSeq(ParsingState state, Mite mite, String property, boolean topLevel) {
    return state.ownMites.find {
      it.cxt == seq &&
      it.contents.multi &&
      (topLevel ? it.contents.second : it.contents.multi) == mite.contents[property]
    }
  }

  private static boolean canJoinSeq(Mite prevSeq) {
    return prevSeq && prevSeq.contents.conj != 'but' && prevSeq.contents.conj != 'and'
  }

  private ParsingState merge(Mite oldMite, Mite newMite, List<Mite> update, Map<Pair<Variable, Variable>, Variable> seqs) {
    def oldArgs = oldMite.contents
    def newArgs = newMite.contents
    def cxt = oldMite.cxt
    if (cxt == elaboration && before[cxt]?.head && oldArgs.elaboration && newArgs.elaboration) {
      return doMerge(oldMite, newArgs, 'elaboration', update, seqs)
    }
    if (cxt == complementizer && oldArgs.content && newArgs.content && before[cxt]?.frame && before[cxt].content) {
      return doMerge(oldMite, newArgs, 'content', update, seqs)
    }
    if (cxt == declOrQuestionComp && oldArgs.comp && newArgs.comp) {
      if (oldArgs.comp == before[complementizer]?.frame &&
          before.visibleMites[declOrQuestionComp]?.any { before[complementizer]?.content == it.contents.head } ) {
        return null
      }
      return doMerge(oldMite, newArgs, 'comp', update, seqs)
    }

    if (cxt in [nom, acc, gen, dat, poDat] && newArgs.noun && !newArgs.head) {
      if (cxt == gen && state.miteList.find { it.cxt == preposition } &&
          !state.findState(oldMite).prevState.miteList.find { it.cxt == preposition }) {
        return null
      }
      return doMerge(oldMite, newArgs, 'noun', update, seqs)
    }
    if (cxt == possessive && oldArgs.possessor && newArgs.possessor && !newArgs.head) {
      return doMerge(oldMite, newArgs, 'possessor', update, seqs)
    }
    if (cxt == boxedForPreposition && oldArgs.prep == newArgs.prep) {
      List<Mite> oldBoxed = oldArgs.boxed
      List<Mite> newBoxed = newArgs.boxed
      def oldAccording = oldBoxed.find { it.cxt == accordingTo }
      def newAccording = newBoxed.find { it.cxt == accordingTo }
      def mergePoint = suggestMergePoint(oldMite, 'boxed')
      if (oldAccording && newAccording && mergePoint) {
        update << boxedForPreposition(prep:oldArgs.prep, boxed:[createMergedMite(oldAccording, 'content', newAccording.contents, seqs, mergePoint.joining)])
        return mergePoint.mergedWith
      }
    }
    if (cxt == accordingTo && oldArgs.content && newArgs.content) {
      def mPoint = doMerge(oldMite, newArgs, 'content', update, seqs)
      update << new Mite(var:newArgs.content, Parser.varCxt(distinguished_in:newArgs.head))
      if (!mPoint.contribution.find { it.cxt == accordingTo } && mPoint.visibleMites[preposition]) {
        return mPoint.hierarchy[1]
      }
      return mPoint
    }
    return null
  }

  private MergePoint suggestMergePoint(Mite oldMite, String prop) {
    def oldMiteState = before.findState(oldMite)
    def prevSeq = findPrevSeq(oldMiteState, oldMite, prop, false)
    if (prevSeq) {
      if (!canJoinSeq(prevSeq)) {
        return null
      }
      return new MergePoint(getFirstMemberState(prevSeq) ?: oldMiteState, true)
    }
    return new MergePoint(oldMiteState, false)
  }

  private ParsingState doMerge(Mite oldMite, Map newArgs, String prop, List<Mite> update, Map<Pair<Variable, Variable>, Variable> seqs) {
    def mPoint = suggestMergePoint(oldMite, prop)
    if (mPoint) {
      update << createMergedMite(oldMite, prop, newArgs, seqs, mPoint.joining)
    }
    return mPoint.mergedWith
  }

  private static Mite createMergedMite(Mite oldMite, String prop, Map newArgs, Map<Pair<Variable, Variable>, Variable> seqs, boolean joining) {
    Variable first = (Variable) oldMite.contents[prop]
    Variable second = newArgs[prop]
    Variable multi = obtainMergedVariable(first, second, seqs, joining)
    return oldMite.cxt.call(newArgs + [(prop):multi, xor:Mite.mergeXor(oldMite.contents, newArgs)])
  }

  private static Variable obtainMergedVariable(Variable first, Variable second, Map<Pair<Variable, Variable>, Variable> seqs, boolean joining) {
    def pair = new Pair(first, second)
    Variable multi = joining ? first : seqs[pair] ?: createMergedVar(first, second)
    seqs[pair] = multi
    return multi
  }

  private static ParsingState getFirstMemberState(Mite prevSeq) {
    return prevSeq.contents.mergedWith
  }


  private static Variable createMergedVar(Variable first, Variable second) {
    return new Variable("$first&$second")
  }

  @Override
  public String toString() {
    return "SeqInterceptor{" + "before=" + before + '}';
  }
}

@TupleConstructor
class MergePoint {
  ParsingState mergedWith
  boolean joining
}