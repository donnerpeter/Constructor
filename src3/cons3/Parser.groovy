package cons3

import groovy.transform.Immutable

/**
 * @author peter
 */
class Parser {

  Chart parse(String text, debug = false) {
    ParsingState state = new ParsingState(chart: new Chart(), situation: new Situation(), participants:[:], constructions:[:], history:FList.emptyList)
    def tokenizer = new StringTokenizer(text, """ '":,.?!""", true)
    for (String w in tokenizer) {
      if (w != ' ') {
        state = handleWord(w.toLowerCase(), state)
        if (debug) {
          println w
          state.constructions.each {
            println "  $it.key -> $it.value"
          }
        }
      }
    }
    return state.chart
  }

  Construction cxt(String name, Closure cl) {
    new Construction(name, cl as Function2)
  }

  private static ParsingState handleCase(Construction caze, ParsingState state, Map args) {
    if (args.save && args.hasNoun) {
      return state.satisfied(caze).restore(args.save).apply((Construction)args.delegate, noun: args.noun)
    }
    return state
  }

  Construction adjective = cxt('adjective') { ParsingState state, Map args -> state.assign(args.nounFrame, args.rel, args.val) }
  Construction nom = cxt('nom') { ParsingState state, Map args ->
    if (args.head?.frame(state.chart)?.type && args.noun) {
      state = state.assign(args.head, 'arg1', args.noun)
      if (args.hasNoun) {
        state = state.satisfied(nom)
      }
    }
    return state
  }
  Construction acc = cxt('acc') { ParsingState state, Map args ->
    def hdType = args.head?.frame(state.chart)?.type
    if (hdType && args.noun) {
      state = state.assign(args.head, 'arg2', args.noun)
      if (args.hasNoun) {
        state = state.satisfied(acc)
      }
    }
    return handleCase(acc, state, args)
  }
  Construction gen = cxt('gen') { ParsingState state, Map args ->
    def type = args.head?.frame(state.chart)?.type
    if (type && args.noun) {
      state = state.assign(args.head,
                           type == 'LACK' ? 'arg2' :
                               'arg1', args.noun)
    }
    return handleCase(gen, state, args)
  }
  Construction instr = cxt('instr') { ParsingState state, Map args ->
    if (args.head?.frame(state.chart)?.type && args.noun) {
      state = state.assign(args.head, 'arg2', args.noun)
    }
    handleCase(instr, state, args)
  }
  Construction dat = cxt('dat') { ParsingState state, Map args ->
    if (args.head && args.noun && args.infinitive) {
      state = state.assign(args.head, 'arg1', args.noun)
    }
    state
  }
  Construction prep = cxt('prep') { ParsingState state, Map args -> handleCase(prep, state, args) }
  Construction sInstr = cxt('sInstr') { ParsingState state, Map args ->
    if (args.head && args.noun) {
      if ( args.noun.frame(state.chart)?.type in ['JOY', 'RELIEF']) {
        state = state.assign(args.head, 'mood', args.noun)
      } else {
        state = state.assign(args.head, 'experiencer', args.noun)
      }
    }

    return state
  }
  Construction poDat = cxt('poDat') { ParsingState state, Map args ->
    def lastNoun = args.noun
    def member = lastNoun?.frame(state.chart)?.f('member')
    if (member) {
      lastNoun = member.var
    }
    def nType = lastNoun?.frame(state.chart)?.type
    if (nType) {
      if (nType in ['OPINION', 'WORDS']) {
        state = state.assign(state.situation, 'opinion_of', args.noun)
      } else if (args.head) {
        state = state.assign(args.head, 'topic', args.noun)
      }
    }

    return state
  }
  Construction oPrep = cxt('oPrep') { ParsingState state, Map args ->
    args.noun ? state.assign(args.head, 'topic', args.noun) : state
  }
  Construction nounGen = cxt('nounGen') { ParsingState state, Map args -> //todo nounGen -> gen
    if (args.noun && args.head) {
      def hType = args.head.frame(state.chart).type
      state = state.assign(args.head, hType == 'WORDS' ? 'author' : hType == 'OPINION' ? 'arg1' : 'criterion', args.noun)
    }
    return state
  }
  Construction kDat = cxt('kDat') { ParsingState state, Map args ->
    args.noun && args.head ? state.assign(args.head, 'goal', args.noun) : state
  }
  Construction prevHistory = cxt('prevHistory') { ParsingState state, Map args ->
    return state
  }
  Construction clauseEllipsis = cxt('clauseEllipsis') { ParsingState state, Map args ->
    return state
  }
  Construction question = cxt('question') { ParsingState state, Map args ->
    if (args.hasComma && !args.comp && args.head) {
      def next = args.comp ?: new Situation()
      state = state.assign(args.head, args.head.frame(state.chart).type == 'FORGET' ? 'theme' : 'question', next).withSituation(next).apply(question, comp:next)
    }
    if (args.questioned && args.comp) {
      if (args.imperative) {
        state = state.assign(state.situation, 'imperative', 'true')
      }
      state = state.assign(args.comp, 'questioned', args.questioned)
    }
    return state
  }
  Construction comeScalarly = cxt('comeScalarly') { ParsingState state, Map args ->
    args.verb && args.order ? state.assign(args.verb, 'type', 'COME_SCALARLY').assign(args.verb, 'order', args.order) : state
  }
  Construction questionVariants = cxt('questionVariants') { ParsingState state, Map args ->
    args.seq && args.questioned ? state.assign(args.questioned, 'variants', args.seq).satisfied(questionVariants) : state
  }
  Construction shortAdjCopula = cxt('shortAdjCopula') { ParsingState state, Map args ->
    if (args.noun && args.pred) {
      state = state.assign(args.noun, 'degree', args.pred)
    }
    return state
  }
  Construction parenthetical = cxt('parenthetical') { ParsingState state, Map args ->
    return state
  }
  Construction preposition = cxt('preposition') { ParsingState state, Map args ->
    return state
  }
  Construction possessive = cxt('possessive') { ParsingState state, Map args ->
    if (args.possessor) {
      def type = args.head?.frame(state.chart)?.type
      if (type) {
        state = state.assign(args.head, type in ['AMAZE', 'PREDICAMENT', 'OPINION'] ? 'arg1' : 'author', args.possessor)
      }
    }
    return state
  }
  Construction control = cxt('control') { ParsingState state, Map args ->
    return args.head && args.slave ? state.assign(args.head, 'theme', args.slave) : state
  }
  Construction declComp = cxt('declComp') { ParsingState state, Map args ->
    if (args.hasComma && !args.comp && args.head && args.complementizer) {
      def next = new Situation()
      state = state.assign(args.head, 'theme', next).withSituation(next).apply(declComp, comp:next)
    }
    return state
  }
  Construction conditionComp = cxt('conditionComp') { ParsingState state, Map args ->
    if (args.hasComma && !args.comp && args.head && args.wh) {
      def next = new Situation()
      state = state.assign(args.head, "${args.wh}Condition", next).withSituation(next).apply(conditionComp, comp:next)
    }
    return state
  }
  Construction reasonComp = cxt('reasonComp') { ParsingState state, Map args ->
    if (args.hasComma && !args.comp && args.head && args.active) {
      def next = new Situation()
      state = state.assign(args.head, "reason", next).withSituation(next)
    }
    return state
  }
  Construction negation = cxt('negation') { ParsingState state, Map args ->
    return state
  }
  Construction also = cxt('also') { ParsingState state, Map args ->
    return state
  }
  Construction vAcc = cxt('vAcc') { ParsingState state, Map args ->
    if (args.head && args.noun) {
      state = state.assign(args.head, args.head.frame(state.chart).type == 'THINK' ? 'theme' : 'goal', args.noun).apply(nom).apply(posleGen) //todo apply everything on every change
    }
    state
  }
  Construction vPrep = cxt('vPrep') { ParsingState state, Map args ->
    if (args.head && args.noun) {
      if (args.head.frame(state.chart).type == 'COME_TO') {
        state = state.assign(args.head, 'domain', args.noun)
      } else {
        state = state.assign(state.situation, 'condition', args.noun)
      }
    }
    state
  }
  Construction posleGen = cxt('posleGen') { ParsingState state, Map args ->
    def type = args.head?.frame(state.chart)?.type
    if (type && args.noun) {
      state = state.assign(args.head,'anchor', args.noun)
    }
    return handleCase(gen, state, args)
  }
  Construction ransheGen = cxt('ransheGen') { ParsingState state, Map args ->
    def type = args.head?.frame(state.chart)?.type
    if (type && args.noun) {
      state = state.assign(args.head,'anchor', args.noun)
    }
    return handleCase(gen, state, args)
  }
  Construction naPrep = cxt('naPrep') { ParsingState state, Map args ->
    args.head && args.noun ? state.assign(args.head, 'location', args.noun) : state
  }
  Construction izGen = cxt('izGen') { ParsingState state, Map args ->
    args.head && args.noun ? state.assign(args.head, 'source', args.noun) : state
  }
  Construction doGen = cxt('doGen') { ParsingState state, Map args ->
    args.head && args.noun ? state.assign(args.head, 'goal', args.noun) : state
  }
  Construction participleArg = cxt('participleArg') { ParsingState state, Map args ->
    args.head && args.participle ? state.assign(args.head, 'theme', args.participle) : state
  }
  Construction seq = cxt('seq') { ParsingState state, Map args ->
    return state
  }
  Construction advObj = cxt('advObj') { ParsingState state, Map args ->
    if (args.head && args.adv) {
      state = state.assign(args.head, 'arg2', args.adv)
    }
    return state
  }
  Construction relTime = cxt('relTime') { ParsingState state, Map args ->
    if (args.head && args.relTime) {
      state = state.assign(args.head, 'relTime', args.relTime)
    }
    return state
  }
  Construction absTime = cxt('absTime') { ParsingState state, Map args ->
    if (args.head && args.rel && args.noun) {
      state = state.assign(args.head, 'relTime_after', args.noun)
    }
    return state
  }
  Construction adverb = cxt('adverb') { ParsingState state, Map args ->
    if (args.head && args.adv) {
      state = state.assign(args.head, 'manner', args.adv)
    }
    return state
  }
  Construction quotedName = cxt('quotedName') { ParsingState state, Map args ->
    if (args.finished) {
      state = state.assign(args.noun, 'name', args.name).satisfied(quotedName)
    }
    return state
  }
  Construction relativeClause = cxt('relativeClause') { ParsingState state, Map args ->
    if (args.noun && args.hasComma && args.wh) {
      def next = new Situation()
      return state.assign(args.noun, 'relative', next).withSituation(next).assign(next, 'wh', args.wh).apply(nestedClause, save: args.save, parent:args.parentSituation)
    }
    return state
  }
  Construction nestedClause = cxt('nestedClause') { ParsingState state, Map args ->
    return state
  }
  Construction directSpeech = cxt('directSpeech') { ParsingState state, Map args ->
    if (args.head && args.hasColon) {
      def message = new Situation()
      state = state.assign(args.head, 'message', message).withSituation(message)
    }

    return state
  }
  Construction summerGarden = cxt('summerGarden') { ParsingState state, Map args ->
    if (args.summer && args.garden) {
      state = state.assign(args.garden, 'name', 'Летний сад')
    }
    return state
  }

  private Update merge(ParsingState state, Construction cxt, Map oldArgs, Map newArgs, Update update, Map<Pair<Variable, Variable>, Variable> seqs) {
    if (cxt in [nom, acc, gen, poDat] && oldArgs.noun && newArgs.noun) {
      return merge(state, cxt, oldArgs, newArgs, 'noun', update, seqs)
    }
    if (cxt in [nom] && oldArgs.head && newArgs.head && !newArgs.noun) {
      return update.addCxt(oldArgs + [head:newArgs.head], cxt, newArgs.init)
    }
    if (cxt == possessive && oldArgs.possessor && newArgs.possessor) {
      return merge(state, cxt, oldArgs, newArgs, 'possessor', update, seqs)
    }

    return update.addCxt(newArgs, cxt)
  }

  Construction seqStart = this.cxt('seqStart') { ParsingState st, Map args ->
    st = st.assign(args.multi, 'member', args.first)
    if (args.conj) {
      st = st.assign(args.multi, 'conj', args.conj)
    }
    if (args.distinguish) {
      st = st.assign(args.second, 'distinguished_in', args.multi)
    }
    st.assign(args.multi, 'member', args.second).satisfied(seqStart)
  }

  private Update merge(ParsingState state, Construction cxt, Map oldArgs, Map newArgs, String prop, Update update, Map<Pair<Variable, Variable>, Variable> seqs) {
    Variable first = (Variable) oldArgs[prop]
    if (first.frame(state.chart).f('member')) {
      return joinSeq(newArgs, state, cxt, first, oldArgs, prop, update)
    }

    def second = newArgs[prop]
    Variable multi = seqs[new Pair(first, second)] ?: new Variable()
    seqs[new Pair(first, second)] = multi
    return update.
            addCxt(seqStart, multi:multi, first:first, second:second, conj:state[seq].conj, distinguish:newArgs.repeated).
            addCxt(oldArgs + [(prop): multi, xor:ParsingState.mergeXor(oldArgs, newArgs)], cxt, ParsingState.mergeInits((Closure)oldArgs.init, (Closure)newArgs.init))
  }

  Construction seqNext = this.cxt('seqNext') { ParsingState st, Map args ->
    if (args.conj) {
      st = st.assign(args.multi, 'conj', args.conj)
    }
    st.assign(args.multi, 'member', args.next).satisfied(seqNext)
  }

  Update joinSeq(Map newArgs, ParsingState state, Construction cxt, Variable seqVar, Map oldArgs, String property, Update update) {
    def newMember = newArgs[property]
    return update.
            addCxt(oldArgs + [(property): seqVar, xor:ParsingState.mergeXor(oldArgs, newArgs)], cxt, ParsingState.mergeInits((Closure)oldArgs.init, (Closure)newArgs.init)).
            addCxt(seqNext, multi:seqVar, next:newMember, conj:state[seq]?.conj)
  }

  ParsingState conjWrap(Map<Construction, Map> constructions, ParsingState state) {
    Update update = new Update(FLinkedMap.emptyMap)
    if (state[seq]) {
      FList<Contribution> history = state[seq].save.history
      def similar = history.findIndexOf { areSimilar(it.apps, constructions) }
      if (similar >= 0) {
        def prev = history[similar].apps
        def seqs = [:]
        for (cxt in constructions.keySet()) {
          if (cxt in prev) {
            update = merge(state, cxt, prev[cxt], constructions[cxt], update, seqs)
          } else {
            update = update.addCxt(constructions[cxt], cxt)
          }
        }

        def modernHistory = state.history
        state = state.clearConstructions().restore(history[similar].before.constructions)

        def lastConj = modernHistory.findIndexOf { it.apps[seq] != null }
        if (lastConj) {
          for (contribution in modernHistory.subList(0, lastConj).reverse()) {
            state = state.apply(contribution.apps)
          }
        }
        return state.apply(update.map)
      }
    }
    for (cxt in constructions.keySet()) {
      update = update.addCxt(constructions[cxt], cxt)
    }
    if (state[clauseEllipsis]) {
      Map<Variable, Variable> mapping = state[clauseEllipsis].mapping
      List<Contribution> prevConstructions = state[clauseEllipsis].remaining
      def index = prevConstructions.findIndexOf { areSimilar(it.apps, update.map) }
      if (index >= 0) {
        state = state.satisfied(clauseEllipsis).assign(state.situation, 'clauseEllipsis', 'true')
        prevConstructions[0..index].each { Contribution oldContribution ->
          oldContribution.apps.each { cxt, upd ->
            upd.each { k, v ->
              if (v instanceof Variable) {
                def nv = update.map[cxt]?.get(k) ?: mapping.get(v)
                if (!nv) {
                  nv = new Variable()
                }
                mapping[v] = nv
              }
            }
          }
        }

        prevConstructions[0..<index].each { Contribution oldContribution ->
          FLinkedMap newContribution = FLinkedMap.emptyMap
          for (cxt in oldContribution.apps.keySet()) {
            def newArgs = [:]
            oldContribution.apps[cxt].each { k, v ->
              newArgs[k] = v instanceof Variable ? mapping[v] : v
            }
            newContribution = newContribution(cxt, newArgs)
          }
          state = state.apply(newContribution)
        }
      }
    }
    return state.apply(update.map)
  }


  ParsingState handleWord(String word, ParsingState state) {
    Tokens t = new Tokens()
    if (Util.parseNumber(word) != null) { //todo generic noun treatment for numbers
      def noun = !state[gen]?.hasNoun && state[gen]?.noun ? state[gen].noun : state.newVariable()
      def init = { it.assign(noun, 'type', word).assign(noun, 'number', 'true') }

      def cases = []
      if (!(state[preposition]?.prep in ['posle', 'ranshe'])) {
        cases << nom
      }
      if (state[preposition]?.prep == 'posle') {
        cases << posleGen
      } else if (state[preposition]?.prep == 'ranshe') {
        cases << ransheGen
      } else {
        cases << gen
      }
      cases << acc

      def qv = state[questionVariants]
      if (qv) {
        def seqVar = state.newVariable()
        state = state.apply((questionVariants):[seq:seqVar], (nom):[:])
        def update = new Update(FLinkedMap.emptyMap)
        cases.each {
          update = joinSeq(state, it, seqVar, noun:noun, init:init, [hasNoun:true, xor:t.a], 'noun', update)
        }
        state = state.apply(update.map)
      } else {
        state = conjWrap(cases.collectEntries { [it, [noun:noun, hasNoun:true, init:init, xor:t.a]] },  state)
      }

      return state
    }

    if (state[quotedName]?.started && !state[quotedName].name) {
      state = state.apply(quotedName, name:word)
    }

    def situation = state.situation
    switch (word) {
      case "удивительный":
        def noun = state.newVariable()
        return state.apply(adjective, nounFrame:noun, rel:'property', val:'AMAZING').apply(nom, noun:noun)
      case "знаменской": // todo a unified treatment for street names
      case "бассейной":
        def noun = state.newVariable()
        def init = { it.assign(noun, 'type', 'STREET') }
        state = conjWrap(state, (gen):[noun:noun, init:init], (adjective):[nounFrame:noun, rel:'name', val:word[0..-3]+"ая", init:init])
        return state
      case "коммерческий":
        def noun = state[acc]?.noun ?: state.newVariable()
        return state.apply(adjective, nounFrame:noun, rel:'kind', val:'COMMERCIAL').apply(acc, noun:noun)
      case "маленький":
        def noun = state[acc]?.noun ?: state.newVariable()
        return state.apply(adjective, nounFrame:noun, rel:'size', val:'LITTLE').apply(acc, noun:noun)
      case "летний":
        def noun = state[acc]?.noun ?: state.newVariable()
        return state.apply((adjective):[nounFrame:noun, rel:'timeAnchor', val:'SUMMER', xor:t.a], (acc):[noun:noun], (summerGarden):[summer:true, xor:t.a])
      case "большим":
        def noun = state[instr]?.noun ?: state.newVariable()
        return state.apply(adjective, nounFrame:noun, rel:'size', val:'BIG').apply(instr, noun:noun)
      case "большой":
        def noun = state[instr]?.noun ?: state[acc]?.noun ?: state.newVariable()
        return state.apply((adjective):[nounFrame:noun, rel:'size', val:'BIG'], (instr):[noun:noun], (acc):[noun:noun])
      case "нашем":
        def we = state.newVariable()
        def possHead = state[prep]?.noun ?: state.newVariable()
        state = state.assign(we, 'type', 'WE')
        return state.apply(possessive, possessor:we, head:possHead)
      case "этому":
        def noun = state[poDat]?.noun ?: state.newVariable()
        return state.apply(adjective, nounFrame:noun, rel:'determiner', val:'THIS').apply(poDat, noun:noun)
      case "всякого":
        def noun = state[gen]?.noun ?: state.newVariable()
        return state.apply(adjective, nounFrame:noun, rel:'determiner', val:'ANY').apply(gen, noun:noun)
      case "скромному":
        def noun = state[poDat]?.noun ?: state.newVariable()
        return state.apply(adjective, nounFrame:noun, rel:'quality', val:'HUMBLE').apply(poDat, noun:noun)
      case "том":
        def noun = state[prep]?.noun ?: state.newVariable()
        return state.apply(adjective, nounFrame:noun, rel:'determiner', val:'THAT').apply(prep, noun:noun)
      case "случай": return noun(state, nom, 'THING') //todo случай=CASE or THING
      case "случае":
        def noun = state[prep]?.noun ?: state.newVariable()
        state = state.apply(prep, noun: noun, hasNoun:true) { it.assign(noun, 'type', 'CASE') }
        return state.apply(conditionComp, head:noun) //todo one noun frame - several cases
      case "удивление": return noun(state, nom, 'AMAZE')
      case "поводу": return noun(state, dat, 'MATTER')
      case "недоумении": return noun(state, prep, 'PREDICAMENT')
      case "рта": return noun(state, gen, 'MOUTH')
      case "смысла": return noun(state, gen, 'MEANING')
      case "молоточек": return noun(state, acc, 'HAMMER')
      case "радостью": return noun(state, instr, 'JOY')
      case "облегчением": return noun(state, instr, 'RELIEF')
      case "улицы": return noun(state, gen, 'STREET')
      case "углу":  //todo plain noun
        def noun = state[prep]?.noun ?: state.newVariable()
        state = state.apply(prep, noun: noun, hasNoun:true) { it.assign(noun, 'type', 'CORNER') }
        return state.apply(gen, head:noun) //todo one noun frame - several cases
      case "магазин":
        def noun = state[acc]?.noun ?: state.newVariable()
        def init = { it.assign(noun, 'type', 'SHOP') }
        state = state.apply((nom):[noun:noun, hasNoun:true, init:init, xor:t.a], (acc):[noun:noun, hasNoun:true, init:init, xor:t.a])
        state = state.apply(naPrep, head:noun)
        state = state.apply(quotedName, noun:noun).satisfied(relativeClause).apply(relativeClause, noun:noun, save:state.constructions)
        return state //todo one noun frame - several cases
      case "сад":
        def noun = state[acc]?.noun ?: state.newVariable()
        def init = { it.assign(noun, 'type', 'GARDEN') }
        state = state.apply((nom):[noun:noun, hasNoun:true, init:init, xor:t.a], (acc):[noun:noun, hasNoun:true, init:init, xor:t.a], (summerGarden):[garden:noun])
        state = state.apply(naPrep, head:noun)
        state = state.satisfied(relativeClause).apply(relativeClause, noun:noun, save:state.constructions)
        return state //todo one noun frame - several cases
      case "сада": return noun(state, gen, 'GARDEN')
      case "магазина":
        def noun = state[gen]?.noun ?: state.newVariable()
        state = state.apply(gen, noun: noun, hasNoun:true) { it.assign(noun, 'type', 'SHOP') }
        state = state.apply(naPrep, head:noun)
        state = state.apply(quotedName, noun:noun).satisfied(relativeClause).apply(relativeClause, noun:noun, save:state.constructions)
        return state //todo one noun frame - several cases
      case "мнению":
        if (state.situation.frame(state.chart).f('opinion_of') && !state[seq]?.conj) {
          def next = new Situation()
          state = state.assign(situation, 'but', next).withSituation(next).apply(prevHistory, history:state)
        }
        def noun = state[poDat]?.noun && !state[poDat]?.hasNoun ? state[poDat].noun : state.newVariable()
        state = state.assign(noun, 'type', 'OPINION')
        return conjWrap(state, (nounGen):[head:noun], (possessive):[head:noun], (poDat):[noun:noun, hasNoun:true, repeated:state[poDat]?.repeated],
                           (parenthetical):[:]) // todo common constructions in по-моему & по моему мнению
      case "словам":
        def noun = state[poDat]?.noun ?: state.newVariable()
        def init = { it.assign(noun, 'type', 'WORDS') }
        state = state.apply((poDat):[noun:noun, hasNoun:true, init:init],
                            (nounGen):[head:noun, init:init],
                            (possessive):[head:noun, init:init])
        return state
      case "случился":
        Variable verb = state.newVariable()
        state = state.assign(verb, 'type', 'HAPPEN').assign(situation, 'time', 'PAST')
        return state.apply(sInstr, head:verb).apply(nom, head:verb)
      case 'со': return preposition(state, sInstr, instr)
      case 'с': return preposition(state, sInstr, instr)
      case 'по':
        return state.apply((preposition):[prep:'по'], (poDat):[noun:new Variable(), repeated:(state[seq] != null)])
      case 'о': return preposition(state, oPrep, prep)
      case 'к': return preposition(state, kDat, dat)
      case 'к': return preposition(state, kDat, dat)
      case 'до': return preposition(state, doGen, gen)
      case 'в':
        def noun = state.newVariable()
        state = state.apply((vAcc):[noun: noun, xor:t.a], (vPrep):[noun: noun, xor:t.a])
        def save = state.constructions
        return state.clearConstructions().apply((acc):[save: save, delegate: vAcc, noun:noun, xor:t.b], (prep):[save: save, delegate: vPrep, noun:noun, xor:t.b])
      case 'из':
      case 'изо': return preposition(state, izGen, gen)
      case 'на': return preposition(state, naPrep, prep)
      case "мной": return noun(state, instr, 'ME')
      case ":":
        if (state[directSpeech]) {
          //todo construction handling of elaboration and direct speech
          return state.apply(directSpeech, hasColon:true)
        }

        def elaboration = new Situation()
        state = state.assign(situation, 'elaboration', elaboration)
        return state.withSituation(elaboration)
      case "я": return noun(state, nom, 'ME')
      case "мне": return noun(state, dat, 'ME')
      case "мы": return noun(state, nom, 'WE')
      case "нам": return noun(state, dat, 'WE')
      case "мое":
        def me = state.newVariable()
        return conjWrap(state, (possessive):[possessor:me, init:{ it.assign(me, 'type', 'ME') }])
      case "моему":
        def me = state.newVariable()
        return conjWrap(state, (possessive):[possessor:me, init:{ it.assign(me, 'type', 'ME') }])
      case "и": return state.apply(seq, save:state, conj:'and')
      case "или": return state.apply(seq, save:state, conj:'or')
      case "а":
        def next = new Situation()
        return state.assign(situation, 'but', next).withSituation(next).apply(prevHistory, history:state)
      case "но":
        return state.assign(situation, 'but', new Situation())
      case "тут":
        return state.assign(situation, 'emphasis', 'true')
      case "потом":
        Update update = new Update((comeScalarly):[order:'AFTER', xor: t.a], (nom):[:])
        if (state[comeScalarly]?.order) {
          state = state.inhibit(comeScalarly)
        } else {
          update = update.addCxt(relTime, relTime:'AFTER', xor:t.a)
        }
        return state.apply(update.map)
      case "все":
        return state.assign(state[nom].noun, 'quantifier', 'ALL')
      case "дальше":
        def adv = state.newVariable()
        return state.apply((comeScalarly):[order:'AFTER', xor: t.a],  (advObj):[adv: adv, xor:t.a, init:{ it.assign(adv, 'type', 'NEXT') }])
      case "их":
        def they = state.newVariable()
        def init = { st -> st.assign(they, 'type', 'THEY') }
        return conjWrap(state, (possessive):[possessor:they, init:init], (acc):[noun:they, hasNoun:true, init:init])
      case "они": return noun(state, nom, 'THEY')
      case "соседям": return noun(state, dat, 'NEIGHBOURS')
      case "кассиршу": return noun(state, acc, 'CASHIER')
      case "кассирша": return noun(state, nom, 'CASHIER') //todo кассир
      case "кассирши": return noun(state, nounGen, 'CASHIER')
      case "одних": return noun(state, nounGen, 'SOME')
      case "других": return noun(state, nounGen, 'OTHERS')
      case "деревья": return noun(state, acc, 'TREES')
      case "деньги": return noun(state, acc, 'MONEY')
      case "ее":
      case "её":
        def she = state.newVariable()
        def init = { st -> st.assign(she, 'type', 'SHE') }
        return conjWrap(state, (possessive):[possessor:she, init:init], (acc):[noun:she, hasNoun:true, init:init])
      case "носом": return noun(state, instr, 'NOSE')
      case "челюстью": return noun(state, instr, 'JAW')
      case "семь": return noun(state, nom, '7')
      case "семи": return noun(state, gen, '7')
      case "восемь": return noun(state, nom, '8')
      case "восьми": return noun(state, gen, '8')
      case "порядок":
        def noun = state.newVariable()
        state = state.apply(acc, noun: noun, hasNoun:true) { it.assign(noun, 'type', 'ORDER') }
        return state.apply(nounGen, head:noun) //todo one noun frame - several cases
      case "слова":
        def noun = state[acc]?.hasNoun ? state.newVariable() : state[acc]?.noun ?: state.newVariable()
        def init = { it.assign(noun, 'type', 'WORDS') }
        state = state.apply((nom):[noun:noun, hasNoun:true, init:init],
                            (acc):[noun:noun, hasNoun:true, init:init],
                            (nounGen):[head:noun, init:init],
                            (possessive):[head:noun, init:init])
        return state
      case "счета": return noun(state, nounGen, 'COUNTING')
      case "счете": return noun(state, prep, 'COUNTING')
      case "работы":
        state = noun(state, gen, 'WORK')
        state = state.apply(absTime, noun:state[posleGen].noun)
        return state
      case "вдруг": return state.apply(adverb, adv:'SUDDENLY')
      case "опять":
        if (state[nom]) {
          def verb = state.newVariable()
          return state.assign(verb, 'anchor', 'AGAIN').apply(nom, head:verb)
        }
        return state
      case "слегка": return state.apply(adverb, adv:'SLIGHTLY')
      case "грустно": return state.apply(adverb, adv:'SADLY')
      case "тоже":
        def alsoVar = state.newVariable()
        def subj = state.newVariable()
        state = state.assign(alsoVar, 'type', 'ALSO')
        state = state.assign(alsoVar, 'arg1', subj)
        return state.apply(also, also:alsoVar, subj:subj)
      case "не":
        return state.apply(negation)
      case "забыл":
        if (state[nom]) {
          Variable verb = state[nom].head ?: state.newVariable()
          state = state.assign(verb, 'type', 'FORGET').assign(situation, 'time', 'PAST')
          return state.apply(question, head:verb).apply(nom, head:verb).apply(adverb, head:verb)
        }
        return state
      case "забыли":
        Variable verb = state.newVariable()
        state = state.assign(verb, 'type', 'FORGET').assign(situation, 'time', 'PAST')
        def subj = state.newVariable()
        return state.apply(advObj, head:verb).apply(acc, head:verb).apply(nom, noun:subj, head:verb) { it.assign(subj, 'type', 'THEY') }
      case "помнят":
        Variable verb = state.newVariable()
        state = state.assign(verb, 'type', 'REMEMBER').assign(situation, 'time', 'PRESENT')
        state = state.apply(acc, head:verb)
        if (!state[nom]) {
          def subj = state.newVariable()
          state = state.apply(nom, noun:subj) { it.assign(subj, 'type', 'THEY') }
        }
        return state.apply(nom, head:verb)
      case "могут":
        def verb = state.newVariable()
        def also = state[also]
        def subj = null
        if (also) {
          subj = also.subj
          state = state.assign(also.also, 'theme', verb)
        }
        if (state[negation] != null) {
          state = state.assign(verb, 'negated', 'true')
        }
        if (!subj) {
          subj = state.newVariable()
        }
        state = state.assign(verb, 'type', 'CAN').assign(situation, 'time', 'PRESENT')
        state = state.assign(subj, 'type', 'THEY')
        return state.apply(control, subj:subj, head:verb)
      case 'стали':
        def verb = state.newVariable()
        state = state.assign(verb, 'type', 'BEGIN').assign(situation, 'time', 'PAST')
        return conjWrap(state, (control):[subj:state[nom], head:verb], (nom):[head:verb])
      case 'начали':
        def verb = state.newVariable()
        state = state.assign(verb, 'type', 'BEGIN').assign(situation, 'time', 'PAST')
        return conjWrap(state, (control):[subj:state[nom], head:verb], (nom):[head:verb])
      case "отправился":
        def verb = state.newVariable()
        state = state.assign(verb, 'type', 'GO_OFF').assign(situation, 'time', 'PAST')
        return conjWrap(state, (kDat):[head:verb], (nom):[head:verb])
      case "пошли":
        def verb = state.newVariable()
        state = state.assign(verb, 'type', 'GO').assign(situation, 'time', 'PAST')
        return conjWrap(state, (nom):[head:verb], (vAcc):[head:verb])
      case "обнаружили":
        if (state[nom]) {
          def verb = new Variable()
          state = state.assign(verb, 'type', 'DISCOVER').assign(situation, 'time', 'PAST').inhibit(relativeClause) //todo generic suppress for context-freeness
          return state.satisfied(declComp).apply(declComp, head:verb).apply(nom, head:verb).apply(adverb, head:verb)
        }
        return state
      case "улыбнулась":
        def verb = state[nom]?.head ?: state.newVariable()
        state = state.assign(verb, 'type', 'SMILE').assign(situation, 'time', 'PAST')
        return conjWrap(state, (nom):[head:verb], (adverb):[head:verb])
      case "сказала":
        def verb = state[nom]?.head ?: state.newVariable()
        state = state.assign(verb, 'type', 'SAY').assign(situation, 'time', 'PAST')
        return conjWrap(state, (nom):[head:verb], (directSpeech):[head:verb])
      case "вынула":
        def verb = state[nom]?.head ?: state.newVariable()
        state = state.assign(verb, 'type', 'TAKE_OUT').assign(situation, 'time', 'PAST')
        return conjWrap(state, (nom):[head:verb], (acc):[head:verb], (izGen):[head:verb])
      case "показались":
        def verb = state[nom]?.head ?: state.newVariable()
        state = state.assign(verb, 'type', 'SEEM').assign(situation, 'time', 'PAST')
        return conjWrap(state, (nom):[head:verb], (dat):[head:verb], (participleArg):[head:verb])
      case "подвигав":
        def verb = state.newVariable()
        state = state.assign(verb, 'type', 'MOVE').assign(verb, 'background', 'perfect')
        return conjWrap(state, (instr):[head:verb], (adverb):[head:verb])
      case "дойдя":
        def verb = state[instr]?.head ?: state.newVariable()
        state = state.assign(verb, 'type', 'COME_TO').assign(verb, 'background', 'perfect')
        return conjWrap(state, (vPrep):[head:verb], (doGen):[head:verb])
      case "вдумываясь":
        def verb = state.newVariable()
        state = state.assign(verb, 'type', 'THINK').assign(verb, 'background', 'present')
        return conjWrap(state, (vAcc):[head:verb])
      case "вспомнить":
        def verb = state.newVariable()
        return infinitive(state, verb, 'RECALL', [(acc):[head:verb]])
      case "делать":
        def verb = state.newVariable()
        return infinitive(state, verb, 'DO', [(acc):[head:verb]])
      case "спорить":
        def verb = state.newVariable()
        return infinitive(state, verb, 'ARGUE', [:])
      case "считать":
        def verb = state.newVariable()
        return infinitive(state, verb, 'COUNT', [(acc):[head:verb]])
      case "поливать":
        def verb = state.newVariable()
        return infinitive(state, verb, 'TO_WATER', [(acc):[head:verb]])
      case "танцевать":
        def verb = state.newVariable()
        return infinitive(state, verb, 'DANCE', [:])
      case "нужно":
        def verb = state.newVariable()
        return state.assign(verb, 'type', 'NEED').apply((acc):[head:verb], (dat):[head:verb, infinitive:true])
      case "спросить":
        def verb = state.newVariable()
        return infinitive(state, verb, 'ASK', [(acc):[head:verb]])
      case "думают":
        if (state[nom]) {
          def verb = state.newVariable()
          state = state.assign(verb, 'type', 'THINK').assign(situation, 'time', 'PRESENT')
          return state.apply(poDat, head:verb).apply(nom, head:verb).apply(acc, head:verb)
        }
        return state
      case "спросил":
      case "спросили":
        def verb = state.newVariable()
        state = state.assign(verb, 'type', 'ASK').assign(situation, 'time', 'PAST')
        return conjWrap(state, (acc):[head:verb], (nom):[head:verb], (question):[head:verb], (oPrep):[head:verb])
      case "делал":
      case "делали":
        def verb = state.newVariable()
        state = state.assign(verb, 'type', 'DO').assign(situation, 'time', 'PAST')
        return conjWrap(state, (acc):[head:verb], (nom):[head:verb])
      case "поблагодарили":
        def verb = state.newVariable()
        state = state.assign(verb, 'type', 'THANK').assign(situation, 'time', 'PAST')
        return conjWrap(state, (acc):[head:verb], (nom):[head:verb])
      case "выбежали":
        def verb = state.newVariable()
        state = state.assign(verb, 'type', 'RUN_OUT').assign(situation, 'time', 'PAST')
        return conjWrap(state, (izGen):[head:verb], (nom):[head:verb], (sInstr):[head:verb])
      case "приуныли":
        def verb = state[nom]?.head ?: state.newVariable()
        state = state.assign(verb, 'type', 'GET_SAD').assign(situation, 'time', 'PAST')
        return conjWrap(state, (nom):[head:verb], (reasonComp):[head:situation], (relTime):[head:state.situation])
      case "остановились":
        def verb = state[nom]?.head ?: state.newVariable()
        state = state.assign(verb, 'type', 'STOP').assign(situation, 'time', 'PAST')
        return conjWrap(state, (nom):[head:verb])
      case ",":
        state = state.apply((declComp):[hasComma:true],
                            (conditionComp):[hasComma:true],
                            (reasonComp):[hasComma:true],
                            (question):[hasComma:true],
                            (relativeClause):[hasComma:true, parentSituation:situation],
                            (prevHistory):[history:state],
                            (seq):[save:state, hasComma:true])
        if (state[nestedClause]) {
          state = state.withSituation(state[nestedClause].parent).clearConstructions().restore(state[nestedClause].save).satisfied(nestedClause)
        }
        if (state[parenthetical]?.hasComma) {
          state = state.satisfied(parenthetical)
        } else {
          state = state.apply(parenthetical, hasComma:true)
        }
        return state
      case "если":
        return state.apply(conditionComp, wh:'if')
      case "когда":
        return state.apply(conditionComp, wh:'when')
      case "Что":
      case "что":
        def noun = state.newVariable()
        state = state.apply(question, questioned:noun)
        if (!state[declComp]) {
          //todo generic noun treatment for что
          state = state.apply((nom):[noun:noun, hasNoun:true, xor:t.a], (acc):[noun:noun, hasNoun:true, xor:t.a])
        }
        state = state.apply(declComp, complementizer:'that')
        if (state[relativeClause]?.hasComma) {
          def wh = new Variable()
          state = state.apply(relativeClause, wh:wh).apply(naPrep, head:wh) //todo pp copula
        }
        return state
      case "идет":
      case "идёт":
        def verb = new Variable()
        state = state.assign(situation, 'time', 'PRESENT')
        def goes = cxt('идёт') { ParsingState st, Map args -> st.assign(verb, 'type', 'GO') }
        return state.inhibit(preposition).apply((comeScalarly):[verb:verb, xor:t.ab], (goes):[verb:verb, xor:t.a],
                                                (vAcc):[head:verb, xor:t.b], (vPrep):[head:verb], (nom):[head:verb],
                                                (posleGen):[head:verb], (ransheGen):[head:verb],
                                                (conditionComp):[head:situation], (absTime):[head:situation])
      case "следовало":
        Variable verb = state.newVariable()
        state = state.assign(situation, 'time', 'PAST')
        return state.inhibit(preposition).apply((nom):[head:verb], (comeScalarly):[verb:verb], (posleGen):[head:verb], (relTime):[head:situation])
      case "раньше":
        return state.apply((comeScalarly):[order:'EARLIER', xor:t.a], (nom):[:], (preposition):[prep:'ranshe'], (relTime):[relTime:'BEFORE', xor:t.a])
      case "после":
        return state.apply((comeScalarly):[order:'AFTER', xor:t.a], (nom):[:], (preposition):[prep:'posle'], (absTime):[rel:'AFTER', xor:t.a])
      case "-":
        if (state[directSpeech]) {
          return state.apply(directSpeech, hasDash:true)
        }
        if (state[question]?.questioned) {
          state = state.clearConstructions().apply(questionVariants, questioned:state[question].questioned)
        }
        if (state[prevHistory]) {
          def mapping = [:]
          FList<Contribution> old = state[prevHistory].history.history
          def modern = state.history[0..state.history.size - old.size]
          def lcs = Util.lcs(old, modern, { a, b -> areSimilar(a.apps, b.apps) } as Function2)
          if (lcs) {
            lcs.each { contribution ->
              contribution.apps.values().each { val ->
                if (val instanceof Variable && !mapping[val]) {
                  mapping[val] = new Variable()
                }
              }
            }
            def commonStart = old.indexOf(lcs[0])
            if (commonStart) {
              FList<Contribution> remaining = FList.fromList(old[0..commonStart-1])
              state = state.apply(clauseEllipsis, remaining:remaining, mapping: mapping).satisfied(prevHistory).inhibit(seq)
            }
          }
        }
        return state
      case ".":
        return state.assign(state.situation, 'dot', 'true').withSituation(new Situation())
      case 'каково':
        def degree = state.newVariable()
        state = state.assign(situation, 'exclamation', degree)
        return state.apply(shortAdjCopula, pred:degree, situation:state.situation)
      case 'был':
        def subj = state[nom]?.noun ?: state.newVariable()
        state = state.assign(situation, 'time', 'PAST').assign(situation, 'copulaTopic', subj)
        return state.apply(naPrep, head: subj).apply(nom, noun:subj)
      case 'было':
        state = state.assign(situation, 'time', 'PAST')
        return state.apply(conditionComp, head:situation)
      case 'так':
        return state.apply(reasonComp, active:true)
      case 'по-моему':
        def opinion = new Variable()
        def me = new Variable()
        return state.assign(state.situation, 'opinion_of', opinion).assign(opinion, 'type', 'OPINION').assign(opinion, 'arg1', me).assign(me, 'type', 'ME').apply(parenthetical)
      case '?': return state.apply(question, comp:situation)
      case '"':
        if (state[quotedName]) {
          if (state[quotedName].started) {
            state = state.apply(quotedName, finished:true)
          } else {
            state = state.apply(quotedName, started:true)
          }
        }
        return state
      case 'лишенными':
        def lack = state.newVariable()
        state = state.assign(lack, 'type', 'LACK')
        return state.apply(participleArg, participle:lack).apply(gen, head:lack)
      case 'бессмысленными':
        def part = state.newVariable()
        state = state.assign(part, 'type', 'MEANINGLESS')
        return state.apply(participleArg, participle:part)
      case '6-ти':
      case 'шести':
        return noun(state, gen, '6')
      case '5-ти':
        return noun(state, gen, '5')
    }
    return state
  }

  private boolean areSimilar(Map<Construction, Map> c1, Map<Construction, Map> c2) {
    def common = c1.keySet().intersect(c2.keySet())

    if (common.size() == 1 && preposition in common) {
      return false
    }
    if (common.size() == 1 && possessive in common && c1[possessive].head) {
      return false
    }

    return common.size() > 0 && common.every { (c1[it].keySet() - 'init' - 'xor') == (c2[it].keySet() - 'init' - 'xor') } &&
            !(prevHistory in common) //todo commas are not particularly important for ellipsis lcs
  }

  private ParsingState infinitive(ParsingState state, Variable verb, String type, LinkedHashMap<Construction, LinkedHashMap<String, Variable>> args) {
    if (state[question]) {
      state = state.apply((question): [mainVerb: verb, imperative: true])
    }
    return state.assign(verb, 'type', type).apply(args + [(dat): [head: verb, infinitive: true], (control): [slave: verb]])
  }

  private ParsingState noun(ParsingState state, Construction caze, String type) {
    return noun(state, caze) { st, n -> st.assign(n, 'type', type) }
  }
  private ParsingState noun(ParsingState state, Construction caze, Closure init) {
    if (state.constructions[caze]?.hasNoun) {
      state = state.inhibit(caze)
    }

    if (state.constructions[preposition]?.prep == 'posle' && caze == gen) {
      caze = posleGen
    }
    if (state.constructions[preposition]?.prep == 'ranshe' && caze == gen) {
      caze = ransheGen
    }
    if (state.constructions[preposition]?.prep == 'по' && caze == dat) {
      caze = poDat
    }

    def noun = state.constructions[caze]?.noun ?: state.newVariable()

    if (noun.frame(state.chart).f('member')) { //todo number agreement
      noun = noun.frame(state.chart).f('member').var
    }

    if (caze == nom) {
      state = state.apply(shortAdjCopula, noun:noun)
    }

    state = state.apply(caze, noun: noun, hasNoun:'true') { init(it, noun) }
    if (state[possessive]) {
      state = state.apply(possessive, head:noun)
    }

    state = state.apply(quotedName, noun:noun).satisfied(relativeClause).apply(relativeClause, noun:noun, save:state.constructions)

    return state
  }

  private ParsingState preposition(ParsingState state, Construction prepCtx, Construction caze) {
    def noun = state.newVariable()
    state = state.apply(prepCtx, noun:noun)
    def save = state.constructions
    return state.clearConstructions().apply(caze, save: save, delegate: prepCtx, noun:noun)
  }

}

@Immutable class Update {
  Map<Construction, Map<String, Object>> map

  Update addCxt(Map newArgs, Construction name, Closure init = null) {
    if (init) {
      newArgs += [init:init]
    }
    return new Update(map + [(name):newArgs])
  }
}