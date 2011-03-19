package cons3

/**
 * @author peter
 */
class Parser {

  Chart parse(String text) {
    def (chart, situation) = new Chart().newSituation()
    ParsingState state = new ParsingState(chart:chart, situation:situation, participants:[:], constructions:[:])
    def tokenizer = new StringTokenizer(text, """ '":,.""", true)
    for (String w in tokenizer) {
      if (w != ' ') {
        state = handleWord(w, state)
      }
    }
    return state.chart
  }

  ParsingState handleWord(String word, ParsingState state) {
    def situation = state.situation
    switch (word) {
      case "Удивительный":
        def (ch, noun) = state.newFrame()
        def (ch1, verb) = ch.newFrame(state.situation)
        return state.apply(ch1, 'adjective', nounFrame:noun.var, rel:'property', val:'AMAZING', rheme:true).apply('nom', noun:noun.var, head:verb)
      case "этому":
        if (state.constructions.poDat) {
          def (ch, noun) = state.newFrame()
          return state.apply(ch, 'adjective', nounFrame:noun.var, rel:'determiner', val:'THIS', rheme:false).apply('poDat', noun:noun.var)
        }
        return state
      case "случай":
        def adj = state.constructions.adjective
        if (adj) {
          Variable noun = adj.nounFrame
          return state.withChart(state.chart.assign(noun, 'type', 'THING', true))
        }
        return state
      case "удивление":
        def poss = state.constructions.possessive
        if (poss) {
          return state.assign(poss.head.frame(state.chart), 'type', 'AMAZE', true).apply('possessive').apply('comp', head:poss.head, rheme:false)
        }
        return state
      case "поводу":
        def adj = state.constructions.adjective
        if (adj) {
          Variable noun = adj.nounFrame
          return state.withChart(state.chart.assign(noun, 'type', 'MATTER', false))
        }
        return state
      case "случился":
        def nom = state.constructions.nom
        if (nom) {
          Variable verb = nom.head
          def ch = state.chart.assign(verb, 'type', 'HAPPEN', true).assign(situation, 'time', 'PAST', false)
          return state.apply('sInstr', head:verb).apply(ch, 'nom', rheme:true)
        }
        return state
      case "мной":
        def sInstr = state.constructions.sInstr
        if (sInstr) {
          def (ch, noun) = state.newFrame()
          ch = ch.assign(noun.var, 'type', 'ME', false)
          return state.apply(ch, 'sInstr', noun:noun.var)
        }
        return state
      case ":":
        def (ch, elaboration) = state.chart.newSituation()
        state = state.withChart(ch).assign(situation, 'elaboration', elaboration, true)
        return state.withSituation(elaboration)
      case "я":
      case "Я":
        def (ch, noun) = state.newFrame()
        def (ch1, verb) = ch.newFrame(situation)
        return state.withChart(ch1).assign(noun, 'type', 'ME', false).apply('nom', noun:noun.var, head:verb)
      case "мое":
        def (ch, noun) = state.newFrame()
        def (ch2, possHead) = state.constructions.whatA ? [ch, state.constructions.whatA.head] : state.constructions.possessive ? [ch, state.constructions.possessive.possHead] : ch.newFrame(state.situation)
        state = state.withChart(ch2).assign(noun, 'type', 'ME', false)
        return state.apply('possessive', possessor:noun.var, head:possHead, conj:state.constructions.possessive)
      case "и": return state.withExpectation(null)
      case "их":
        def (ch, noun) = state.newFrame()
        def (ch1, verb) = state.constructions.acc ? [ch, state.constructions.acc.head] : ch.newFrame(state.situation)
        def (ch2, possHead) = state.constructions.whatA ? [ch1, state.constructions.whatA.head] : ch1.newFrame(state.situation)
        state = state.withChart(ch2).assign(noun, 'type', 'THEY', false).apply('acc', noun:noun.var, head:verb)
        return state.apply('possessive', possessor:noun.var, head:possHead)
      case "они":
        def nom = state.constructions.nom
        def (ch, noun) = state.newFrame()
        def (ch1, head) = nom?.noun ? [ch, nom.head] : ch.newFrame(state.situation)
        ch1 = ch1.assign(noun.var, 'type', 'THEY', false)
        return state.withChart(ch1).apply('nom', noun:noun.var, head: head)
      case "соседям":
        def kDat = state.constructions.kDat
        if (kDat) {
          def (ch, noun) = state.newFrame()
          ch = ch.assign(noun.var, 'type', 'NEIGHBOURS', false)
          return state.apply(ch, 'kDat', noun:noun.var)
        }
        return state
      case "порядок":
        def acc = state.constructions.acc
        if (acc) {
          def (ch, noun) = state.newFrame()
          ch = ch.assign(noun.var, 'type', 'ORDER', false)
          return state.apply(ch, 'acc', noun:noun.var, rheme:false).apply('nounGen', head:noun.var)
        }
        return state
      case "счета":
        def nounGen = state.constructions.nounGen
        if (nounGen) {
          def (ch, noun) = state.newFrame()
          ch = ch.assign(noun.var, 'type', 'COUNTING', false)
          return state.apply(ch, 'nounGen', noun:noun.var)
        }
        return state
      case "вдруг":
        def nom = state.constructions.nom
        if (nom) {
          return state.assign(nom.head.frame(state.chart), 'manner', 'SUDDENLY', true)
        }
        return state
      case "тоже":
        def (ch, also) = state.newFrame()
        def (ch1, subj) = ch.newFrame(situation)
        ch1 = ch1.assign(also.var, 'type', 'ALSO', true)
        ch1 = ch1.assign(also.var, 'arg1', subj, true)
        return state.withChart(ch1).apply(ch1, 'also', also:also.var, subj:subj)
      case "не":
        return state.apply('negation')
      case "забыл":
        def nom = state.constructions.nom
        if (nom) {
          Variable verb = nom.head
          def ch = state.chart.assign(verb, 'type', 'FORGET', true).assign(situation, 'time', 'PAST', false)
          return state.apply('comp', head:verb, rheme:true).apply(ch, 'nom', rheme:true)
        }
        return state
      case "могут":
        def (ch, verb) = state.newFrame()
        state = state.withChart(ch)
        def also = state.constructions.also
        def subj
        if (also) {
          subj = also.subj
          state = state.assign(also.also, 'theme', verb.var, true)
        }
        if (state.constructions.negation != null) {
          state = state.assign(verb.var, 'negated', 'true', false)
        }
        if (!subj) {
          (ch, subj) = state.newFrame()
          state = state.withChart(ch)
        }
        state = state.assign(verb, 'type', 'CAN', false).assign(situation, 'time', 'PRESENT', false)
        state = state.assign(subj, 'type', 'THEY', false)
        return state.apply('control', subj:subj, head:verb.var)
      case "отправился":
        def nom = state.constructions.nom
        if (nom) {
          def verb = nom.head
          def ch = state.chart.assign(verb, 'type', 'GO_OFF', true).assign(situation, 'time', 'PAST', false)
          return state.apply('kDat', head:verb).apply(ch, 'nom', rheme:true, head:verb)
        }
        return state
      case "обнаружили":
        def nom = state.constructions.nom
        if (nom) {
          def verb = nom.head
          def ch = state.chart.assign(verb, 'type', 'DISCOVER', true).assign(situation, 'time', 'PAST', false)
          return state.apply('comp', head:verb, rheme:true).apply(ch, 'nom', rheme:true, head:verb)
        }
        return state
      case "вспомнить":
        def (ch, verb) = state.newFrame()
        state = state.withChart(ch).apply('control', slave:verb.var)
        return state.assign(verb, 'type', 'REMEMBER', false).apply('acc', head:verb.var)
      case "думают":
        def nom = state.constructions.nom
        if (nom) {
          def verb = nom.head
          def ch = state.chart.assign(verb, 'type', 'THINK', false).assign(situation, 'time', 'PRESENT', false)
          return state.apply('poDat', head:verb).apply(ch, 'nom', rheme:false, head:verb).apply('acc', rheme:false)
        }
        return state
      case "спросил":
        def nom = state.constructions.nom
        if (nom) {
          def (ch, verb) = state.newFrame()
          ch = ch.assign(verb.var, 'type', 'ASK', true)//todo don't reassign tense .assign(situation, 'time', 'PAST', false)
          return state.apply('acc', head:verb.var, rheme:true).apply(ch, 'nom', rheme:true, head:verb.var).apply('comp', head:verb.var, rheme:true)
        }
        return state
      case ",":
        def (ch, next) = state.chart.newSituation()
        state = state.withChart(ch)
        def comp = state.constructions.comp
        if (comp) {
          return state.apply('comp', comp:next).withSituation(next).apply(comp.head.frame(ch).type in ['DISCOVER', 'AMAZE'] ? 'declComp' : 'question', situation:next)
        }
        return state
      case "что":
        if (state.constructions.question) {
          def (ch, noun) = state.newFrame()
          def (ch1, verb) = ch.newFrame(state.situation)
          return state.apply(ch1, 'nom', noun:noun.var, head:verb, rheme:false).apply('acc', noun:noun.var, head:verb, rheme:false).apply('question', questioned:noun.var)
        }
        return state
      case "идет":
        def nom = state.constructions.nom
        if (nom) {
          Variable verb = nom.head
          def ch = state.chart.assign(situation, 'time', 'PRESENT', false)
          return state.apply(ch, 'nom').apply('comeScalarly', verb:verb)
        }
        return state
      case "раньше":
        def cs = state.constructions.comeScalarly
        if (cs) {
          return state.apply('comeScalarly', order:'EARLIER').apply('nom')
        }
        return state
      case "-":
        if (state.constructions.question) {
          return state.apply('questionVariants', questioned:state.constructions.question.questioned)
        }
        return state
      case "7":
      case "8":
        if (state.constructions.questionVariants) {
          return state.apply('questionVariants', variant:word)
        }
        return state
      case 'Каково':
        def (ch, degree) = state.newFrame()
        def (ch1, noun) = ch.newFrame(state.situation)
        state = state.withChart(ch1)
        state = state.assign(situation, 'exclamation', degree, true)
        return state.assign(noun, 'degree', degree, true).apply('whatA', degree:degree, head:noun, situation:situation)
      case 'было':
        if (state.constructions.whatA) {
          return state.apply('whatA', time:'PAST')
        }
        return state.assign(situation, 'time', 'PAST' , false)
    }
    return state
  }

}