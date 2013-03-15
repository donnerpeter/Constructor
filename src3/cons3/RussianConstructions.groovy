package cons3
import static cons3.Construction.cxt
import static cons3.Util.*
/**
 * @author peter
 */
interface RussianConstructions {

  Construction nom = cxt('nom', ['noun', 'head']) { ParsingState state, Map args ->
    state
  }.satisfiedWhen { Mite mite ->
    mite.contents.head && mite.contents.noun
  }.routeWrong { Mite mite, ParsingState state ->
    if (mite.contents.head && !mite.v('noun')?.hard && state.network.isChosen(mite)) {
      def prev = state.findState(mite).prevState
      def subj = prev?.miteList?.find { mite.isUnifiable(it) && it.v('noun')?.hard }
      if (subj && prev.miteList.find { it.cxt == seq && it.contents.second == subj.contents.noun && it.contents.conj == ',' && state.network.contradict(it, subj) }) {
        return true
      }
    }
  }.enrichingMites { Mite mite, List<Mite> contribution, ParsingState state ->
    mite.unifyWherePossible(contribution)
  }.structural { mite, state, via, up ->
    Util.showCommonCaseHierarchy(up, state, mite)
  }
  Construction acc = commonCase('acc')
  Construction dat = commonCase('dat')
  Construction prep = commonCase('prep')
  Construction instr = commonCase('instr')
  Construction gen = commonCase('gen')

  Construction adjective = cxt('adjective') { ParsingState state, Map args -> state.assign(args.nounFrame, (String)args.rel, args.val) }
  Construction nomSubject = cxt('nomSubject', ['head', 'noun']) { ParsingState state, Map args ->
    state.assign(args.head, 'arg1', args.noun)
  }
  Construction accArg2 = commonSimpleArg('acc', 'arg2')
  Construction genArg1 = commonSimpleArg('gen', 'arg1')
  Construction genArg2 = commonSimpleArg('gen', 'arg2')
  Construction genCriterion = commonSimpleArg('gen', 'criterion')
  Construction genAuthor = commonSimpleArg('gen', 'author')
  Construction instrArg2 = commonSimpleArg('instr', 'arg2')
  Construction datArg1 = commonSimpleArg('dat', 'arg1')
  Construction datAddressee = commonSimpleArg('dat', 'addressee')
  Construction sInstr = commonPrep('s', instr)
  Construction sInstrExperiencer = commonSimpleArg('sInstr', 'experiencer')
  Construction sInstrMood = commonSimpleArg('sInstr', 'mood')
  Construction sGen = commonPrep('s', gen)
  Construction sGenSource = commonSimpleArg('sGen', 'source')
  Construction poDat = commonPrep('po', dat)
  Construction poDatTopic = commonSimpleArg('poDat', 'topic')
  Construction poDatGoal = commonSimpleArg('poDat', 'goal')
  Construction poDatLuck = commaSurrounded(cxt("poDatLuck", ['content', 'head']) { ParsingState state, Map args ->
    state.assign(args.content, 'topic', args.head)
  })
  Construction oPrep = commonPrep('o', prep)
  Construction oPrepTopic = commonSimpleArg('oPrep', 'topic')
  Construction kDat = commonPrep('k', dat)
  Construction kDatGoal = commonSimpleArg('kDat', 'goal')
  Construction uGen = commonSimpleArg('uGen', 'owner')
  Construction dativePart = cxt('dativePart', ['head', 'dat', 'acc']) { state, args ->
    state.assign(args.acc, 'arg1', args.dat)
  }.enrichingMites { Mite mite, List<Mite> contribution, ParsingState state ->
    if (!mite.contents.acc) {
      def realAcc = contribution.find { it.cxt == acc && !it.contents.head && it.contents.noun }
      if (realAcc) {
        return [new Mite(mite.contents + [acc:realAcc.contents.noun], dativePart)]
      }
    }
    return []
  }
  Construction clauseEllipsis = emptyCxt('clauseEllipsis').enrichingMites { Mite mite, List<Mite> contribution, ParsingState state ->
    if (mite.contents.prevHistory && contribution.find { it.cxt == clauseEllipsis && (it.contents.interceptor || it.contents.but) }) {
      return mite.unifyWherePossible(contribution)
    }
    return []
  }
  Construction declOrQuestionComp = new DeclOrQuestionComp()
  Construction modality = cxt('modality', ['modality', 'infinitive', 'questioned']) { ParsingState state, Map args ->
    state = state.assign(args.modality, 'type', 'modality').assign(args.modality, 'arg1', args.infinitive)
    return state
  }
  Construction timedModality = cxt('timedModality', ['modality', 'time']) { ParsingState state, Map args ->
    state = state.assign(args.modality, 'time', args.time)
    return state
  }
  Construction question = new Question()
  Construction comeScalarly = cxt('comeScalarly', ['verb']) { ParsingState state, Map args ->
    state = state.assign(args.verb, 'type', 'COME_SCALARLY')
    args.order ? state.assign(args.verb, 'order', args.order) : state
  }.satisfiedWhen { Mite mite -> !mite.contents.order || mite.contents.verb }
  Construction questionVariants = cxt('questionVariants', ['seq', 'questioned']) { ParsingState state, Map args ->
    state.assign(args.questioned, 'variants', args.seq)
  }.enrichingMites { Mite mite, List<Mite> contribution, ParsingState state ->
    def sh = contribution.find { it.cxt == sentenceHolder }
    if (sh) {
      return sh.unifyWherePossible(state.findState(mite.firstAtom).prevState)
    }
    return mite.unifyWherePossible(contribution)
  }.structural { mite, state, via, up -> if (mite.contents.questioned) return [ParsingState.EMPTY] }
  Construction reflexiveHolder = emptyCxt('reflexiveHolder')
  Construction shortAdj = cxt('shortAdj', ['noun', 'value', 'copula']) { ParsingState state, Map args ->
    if (args.noun && args.value && args.copula) {
      state = state.assign(args.copula, 'type', 'degree').assign(args.copula, 'arg1', args.noun).assign(args.copula, 'arg2', args.value)
      if (args.time) {
        state = state.assign(args.copula, 'time', args.time)
      }
    }
    return state
  }.structural { mite, state, via, up -> state.findState(mite, 'value')?.hierarchy }
  Construction parenthetical = emptyCxt('parenthetical')
  Construction preposition = new Preposition()
  Construction boxedForPreposition = emptyCxt('boxedForPreposition')
  Construction possArg1 = cxt('possArg1', ['head', 'possessor']) { ParsingState state, Map args ->
    state.assign(args.head, 'arg1', args.possessor)
  }
  Construction possOwner = cxt('possOwner', ['head', 'possessor']) { ParsingState state, Map args ->
    state.assign(args.head, 'owner', args.possessor)
  }
  Construction possAuthor = cxt('possAuthor', ['head', 'possessor']) { ParsingState state, Map args ->
    state.assign(args.head, 'author', args.possessor)
  }
  Construction possessive = cxt('possessive', ['possessor', 'head']) { ParsingState state, Map args ->
    state
  }.enrichingMites { Mite mite, List<Mite> contribution, ParsingState state ->
    if (!mite.v('head')?.hard && !contribution.find { it.cxt == possessive }) {
      def nounFrame = contribution.find { it.cxt == adjective }?.contents?.nounFrame
      if (nounFrame) {
        return [mite.unify(new Mite(possessive, head:nounFrame))]
      }
    }
    return mite.unifyWherePossible(contribution)
  }.satisfiedWhen { it.v('possessor')?.hard && it.contents.head }
  Construction control = cxt('control', ['head', 'slave']) { ParsingState state, Map args ->
    state.assign(args.head, 'theme', args.slave)
  }.structural { mite, state, via, up -> state.findState(mite, 'head')?.hierarchy }
  Construction conditionComp = cxt('conditionComp', ['hasComma', 'head', 'wh']) { ParsingState state, Map args ->
    state.assign(args.head, "${args.wh}Condition", args.comp)
  }.enrichingMites { Mite mite, List<Mite> contribution, ParsingState state ->
    if (contribution.find { it.cxt == comma }) {
      return [mite.unify(new Mite(conditionComp, hasComma:true))]
    }
    if ((mite.contents.hasComma || mite.contents.wh) && contribution.find { it.cxt == conditionComp && it.contents.head }) {
      return []
    }
    return mite.unifyWherePossible(contribution)
  }.structural { mite, state, via, up ->
    if (mite.contents.wh) {
      return [ParsingState.EMPTY]
    }
  }
  Construction reasonComp = cxt('reasonComp', ['reason', 'head']) { ParsingState state, Map args ->
    return state.assign(args.head, 'reason', args.reason)
  }.enrichingMites { Mite mite, List<Mite> contribution, ParsingState state ->
    if (mite.contents.head && contribution.find { it.cxt == reasonComp && it.contents.hasComma } ||
        mite.contents.head && mite.contents.hasComma && contribution.find { it.cxt == reasonComp && it.contents.reason }) {
      return mite.unifyWherePossible(contribution)
    }
    return []
  }.structural { mite, state, via, up ->
    if (mite.contents.hasComma) {
      return [ParsingState.EMPTY]
    }
  }
  Construction numQuantifier = cxt('numQuantifier', ['num', 'noun']) { ParsingState state, Map args ->
    state.assign(args.noun, 'quantifier', args.num)
  }.enrichingMites { Mite mite, List<Mite> contribution, ParsingState state -> NumQuantifier.enrichUpdate(mite, contribution, state) }
  Construction negation = cxt('negation', ['negated']) { ParsingState state, Map args ->
    state.assign(args.negated, 'negated', 'true')
  }.enrichingMites { Mite mite, List<Mite> contribution, ParsingState state ->
    if (!mite.contents.negated) {
      def vh = contribution.find { it.cxt == verbHolder && ((Variable) it.contents.head)?.hard }
      if (vh) {
        return [mite.unify(new Mite(negation, negated:vh.contents.head))]
      }
      def comp = contribution.find { it.cxt == declOrQuestionComp && ((Variable) it.contents.comp)?.hard }
      if (comp) {
        return [mite.unify(new Mite(negation, negated:comp.contents.comp))]
      }
    }
    []
  }
  Construction also = cxt('also', ['hasAlso', 'head']) { ParsingState state, Map args ->
    state.assign(args.head, 'also', 'true')
  }
  Construction vAcc = commonPrep('v', acc)
  Construction vAccGoal = commonSimpleArg('vAcc', 'goal')
  Construction vAccTheme = commonSimpleArg('vAcc', 'theme')
  Construction vPrepDomain = commonSimpleArg('vPrep', 'domain')
  Construction vPrepCondition = commonSimpleArg('vPrep', 'condition')
  Construction vPrep = commonPrep('v', prep)
  Construction posleGen = commonSimpleArg('posleGen', 'anchor').satisfiedWhen { it.contents.noun && it.contents.head }
  Construction ransheGen = commonSimpleArg('ransheGen', 'anchor').satisfiedWhen { it.contents.noun && it.contents.head }
  Construction naPrep = commonSimpleArg('naPrep', 'location').satisfiedWhen { it.contents.noun && it.contents.head }
  Construction izGen = commonPrep('iz', gen)
  Construction izGenSource = commonSimpleArg('izGen', 'source')
  Construction doGen = commonPrep('do', gen)
  Construction doGenGoal = commonSimpleArg('doGen', 'goal')
  Construction otGen = commonPrep('ot', gen)
  Construction otGenTheme = commonSimpleArg('otGen', 'theme')
  Construction participleArg = cxt('participleArg', ['participle', 'head']) { ParsingState state, Map args ->
    args.head && args.participle ? state.assign(args.head, 'theme', args.participle) : state
  }
  Construction adverbialPhrase = commaSurrounded(cxt('adverbialPhrase', ['content', 'head']) { ParsingState state, Map args ->
    state.assign(args.head, 'perfectBackground', args.content)
  })
  Construction accordingTo = commaSurrounded(cxt('accordingTo', ['content', 'head']) { ParsingState state, Map args ->
    state.assign(args.head, 'opinion_of', args.content)
  }).routeWrong { Mite mite, ParsingState state ->
    if (mite.v('head').frame(state.chart).allAssignments('opinion_of').size() > 1) {
      return true
    }
  }.satisfiedWhen { Mite mite -> mite.contents.head && mite.contents.content }
  Construction seq = new SeqConstruction()
  Construction advObj = cxt('advObj', ['adv', 'head']) { ParsingState state, Map args ->
    if (args.head && args.adv) {
      state = state.assign(args.head, 'arg2', args.adv)
    }
    return state
  }
  Construction verbEmphasis = cxt('verbEmphasis', ['verb', 'emphasis']) { ParsingState state, Map args ->
    state.assign(args.verb, (String)args.emphasis, 'true')
  }
  Construction relTime = cxt('relTime', ['relTime', 'head']) { ParsingState state, Map args ->
    state.assign(args.head, 'relTime', args.relTime)
  }.satisfiedWhen { it.contents.head }
  Construction absTime = cxt('absTime', ['rel', 'head', 'noun']) { ParsingState state, Map args ->
    state.assign(args.head, 'relTime_after', args.noun)
  }
  Construction adverb = cxt('adverb', ['head', 'adv', 'attr']) { ParsingState state, Map args ->
    state.assign(args.head, (String)args.attr, args.adv)
  }
  Construction quotedName = cxt('quotedName', ['noun', 'name']) { ParsingState state, Map args ->
    state.assign(args.noun, 'name', args.name)
  }.enrichingMites { Mite mite, List<Mite> contribution, ParsingState state ->
    if (contribution.find { it.cxt == quote }) {
      if (mite.contents.noun && !mite.contents.quote1) {
        return [mite.unify(RussianConstructions.quotedName(quote1:true))]
      }
      if (mite.contents.quote1 && !mite.contents.quote2) {
        return [mite.unify(RussianConstructions.quotedName(quote2:true))]
      }
    }
    def word = contribution.find { it.cxt == word }
    if (word && mite.contents.quote1) {
      return [mite.unify(RussianConstructions.quotedName(name:word.contents.word))]
    }
    return []
  }
  Construction complementizer = new Complementizer()
  Construction directSpeech = cxt('directSpeech', ['head', 'message']) { ParsingState state, Map args ->
    state.assign(args.head, 'message', args.message).assign(args.message, 'directSpeech', 'true')
  }.structural { mite, state, via, up -> mite.contents.head ? [ParsingState.EMPTY] : null }.satisfiedWhen { Mite mite ->
    mite.contents.head && mite.contents.message
  }
  Construction summerGarden = cxt('summerGarden', ['summer', 'garden']) { ParsingState state, Map args ->
    state.assign(args.garden, 'name', 'Летний сад')
  }
  Construction subjunctive = cxt('subjunctive', ['head', 'subjunctive']) { ParsingState state, Map args ->
    state.assign(args.head, 'irrealis', 'true')
  }
  Construction adverbPred = cxt('adverbPred', ['adverb', 'time']) { ParsingState state, Map args ->
    state.assign(args.adverb, 'time', args.time)
  }
  Construction verbHolder = emptyCxt('verbHolder').enrichingMites { Mite mite, List<Mite> contribution, ParsingState state ->
    if (mite.atom && !state.network.isChosen(mite) && state.visibleMites[verbHolder]?.find { !it.atom }) return []
    if (mite.contents.hasColon) return []

    if (contribution.find { it.cxt == verbHolder && it.contents.newClause }) return []

    def hasHead = ((Variable) mite.contents.head)?.hard
    if (hasHead && contribution.find { it.cxt == seq && it.contents.conj in ['and', 'but'] }) {
      return []
    }

    return mite.unifyWherePossible(contribution)
  }
  Construction sentenceHolder = emptyCxt('sentenceHolder').
          enrichingMites { Mite mite, List<Mite> contribution, ParsingState state ->
            if (mite.contents.mustFinish) {
              return []
            }
            return mite.unifyWherePossible(contribution.findAll { it.cxt == sentenceHolder && !it.contents.mustStart })
          }
  Construction elaboration = cxt('elaboration', ['head', 'elaboration']) { ParsingState state, Map args ->
    state.assign(args.head, 'elaboration', args.elaboration)
  }.structural { mite, state, via, up ->
    if (mite.contents.head) {
      return [ParsingState.EMPTY]
    }
    return null

  }
  Construction dot = cxt('dot', ['head']) { ParsingState state, Map args ->
    state.assign(args.head, 'dot', 'true')
  }.enrichingMites { Mite mite, List<Mite> contribution, ParsingState state -> [] }.structural { mite, state, via, up -> [ParsingState.EMPTY] }
  Construction comma = emptyCxt('comma').enrichingMites { Mite mite, List<Mite> contribution, ParsingState state -> [] }
  Construction colon = emptyCxt('comma').enrichingMites { Mite mite, List<Mite> contribution, ParsingState state -> [] }
  Construction word = emptyCxt('word')
  Construction quote = emptyCxt('quote').enrichingMites { Mite mite, List<Mite> contribution, ParsingState state -> [] }


  Construction verbalModifier = emptyCxt('verbalModifier').structural { mite, state, via, up ->
    if (!up && ((Variable) mite.lastAtom.contents.head)?.hard) {
      return state.findState(mite, 'hasModifier')?.prevVisibleState?.hierarchy
    }
  }

  Construction argSharing = emptyCxt('argSharing')
  Construction filler = new Filler()
  Construction copula = cxt('copula', ['head']) { ParsingState state, Map args ->
    state.assign(args.head, 'type', 'copula')
  }


}
