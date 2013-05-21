package cons4

/**
 * @author peter
 */
class EnglishGenerator {

  String generate(Chart chart) {
    def generator = new StatefulGenerator()
    generator.generate(chart)

    String result = ""
    for (w in generator.output) {
      if (!result.empty && !(w in ['.', ',', ';', ':', '\n-', "'s", '?'])) {
        result += " "
      }
      result += w
    }
    if (result.endsWith(',')) {
      result = result[0..-2]
    }

    return result
  }

}

class StatefulGenerator {
  boolean capitalize = true
  List<String> output = []
  boolean past

  private void out(String word) {
    assert word
    if (word == ',') {
      if (output.empty || output[-1] == ',') return
    }
    if (word == ':' && output[-1] == ',') {
      output[-1] = word
      return
    }

    if (word.startsWith("'")) {
      output[-1] = output[-1] + word
      return
    }

    output << (capitalize ? word.capitalize() : word)
    capitalize = false
  }

  private class ClauseGenerator {
    Frame frame
    boolean topLevel
    Frame comp
    Map<String, Frame> preAdjuncts = [:]
    Map<String, Frame> postAdjuncts = [:]
    List<ClauseGenerator> members = []
    Frame wrapper

    ClauseGenerator(Frame frame, boolean topLevel, Frame comp) {
      this.frame = frame
      this.topLevel = topLevel
      this.comp = comp

      if (frame?.f('relTime_after')) {
        def anchor = frame.f('relTime_after')
        Map<String, Frame> toAdd = frame.chart.earlier(anchor, 'type', frame, 'type') || frame.chart.earlier(anchor, 'name', frame, 'type') ? preAdjuncts : postAdjuncts
        toAdd.relTime_after = anchor
      }
    }

    def generate(boolean last) {
      if (frame?.f('member')) {
        List<ClauseGenerator> conjuncts = frame.flatten().collect { new ClauseGenerator(it, false, null) }
        generateConjuncts(conjuncts) { gen, lst -> gen.generate(lst) }
        if (topLevel) {
          finishSentence(conjuncts[-1])
        }
        return
      }

      if (frame && frame.usages('topic').find { it.type == 'LUCK' }) {
        out 'by some sheer luck,'
      }
      else if (frame?.s('emphasis')) {
        out 'there'
      } else if (frame?.s('relTime') == 'AFTER') { //todo a generic situation time frame
        out 'then'
      } else if (frame?.s('relTime') == 'BEFORE') {
        out 'before'
        out ','
      }

      for (k in preAdjuncts.keySet()) {
        if (k == 'relTime_after') {
          out 'after'
          np(preAdjuncts[k], false)
          out ','
        }
      }

      def bg = frame?.f('perfectBackground')
      if (bg && bg.chart.earlier(bg, 'type', frame, 'type')) {
        generateBackground(bg)
        out ','
      }

      if (frame) {
        Frame subj = clauseInner(frame, true, bg != null, topLevel)
        if (bg && bg.chart.earlier(frame, 'type', bg, 'type')) {
          out ','
          generateBackground(bg)
        }
        if (last) {
          generateCondition(frame)
          generateElaboration(frame)
          generateReason(frame)
          generateQuestionVariants(subj)
          generateDirectSpeech(frame)
        }
      }

      if (comp) {
        out 'to'
        out 'consult'
        np(comp.f('arg2'), false)
        out 'on'
        np(comp.f('topic'), false)
      }

      for (k in postAdjuncts.keySet()) {
        if (k == 'relTime_after') {
          out 'after'
          np(postAdjuncts[k], false)
        }
      }
      if (topLevel) {
        finishSentence(this)
      }
    }

    private void generateBackground(Frame bg) {
      if (bg.type == 'MOVE') {
        out 'moving'
        np(bg.f('arg2'), false)
        emitMoveTail(bg)
        out ','
      } else if (bg.type == 'THINK') {
        out ','
        out 'thinking'
        out 'carefully'
        out 'about'
        np(bg.f('theme'), false)
      } else if (bg.type == 'COME_TO') {
        out 'reaching'
        np(bg.f('goal'), false)
        if (bg.f('domain')?.type == 'COUNTING') {
          out 'in'
          out 'count'
        }
      }
    }
  }

  def generate(Chart chart) {
    List<ClauseGenerator> generators = planClauses(chart)

    for (clause in generators) {
      capitalize = true
      if (clause?.frame?.flatten()?.get(0)?.s('butEmphasis')) {
        out 'but'
      }
      else if (clause?.frame?.s('andEmphasis')) {
        out 'and'
      }
      clause.generate(true)
    }
  }

  private Frame findLastVerb(ClauseGenerator clause) {
    def last = clause.frame
    while (true) {
      if (last?.type in ['DISCOVER'] && last.f('theme')) {
        last = last.f('theme')
      }
      else if (last?.type in ['FORGET'] && last.f('arg2') && last.f('arg2').flatten().any { it.type in ['fact', 'question']}) {
        last = last.f('arg2')
      }
      else if (last?.type in ['ASK']) {
        last = last.f('question')
      }
      else if (last?.type == 'modality') {
        last = last.f('arg1')
      }
      else if (last?.type in ['fact', 'question']) {
        last = last.f('content')
      }
      else if (last?.f('member')) {
        last = last.f('member')
      }
      else if (last?.f('whenCondition')) {
        last = last.f('whenCondition')
      }
      else if (last?.f('reason')) {
        last = last.f('reason')
      }
      else if (last?.f('ifCondition')) {
        last = last.f('ifCondition')
      }
      else break
    }
    return last
  }

  private void finishSentence(ClauseGenerator clause) {
    Frame last = findLastVerb(clause)
    if (last?.s('dot') == 'true') {
      out '.'
    }
    else if (clause.frame?.type == 'question' || findQuestioned(clause.frame) && clause.frame.type != 'degree') {
      out '?'
    }
  }

  private void generateConjuncts(List<ClauseGenerator> conjuncts, Closure generator) {
    Frame seq = conjuncts[0].frame.usages('member')[0]
    for (int j in 0..<conjuncts.size()) {
      ClauseGenerator cur = conjuncts[j]
      if (j > 0 && j == conjuncts.size() - 1) {
        String conj = seq?.s('conj')
        if (conj == 'but') {
          if (cur.frame.s('negated')) {
            out 'and'
          } else {
            def prev = conjuncts[j - 1]
            out(prev.frame.f('opinion_of') && cur.frame.f('opinion_of') ? ';' : ',')
            out(prev.frame.s('irrealis') == 'true' ? 'when' : 'but')
          }
        } else if (conj == 'and' || !seq) {
          if (conjuncts.size() > 2 || cur.frame.f('perfectBackground')) {
            out ','
          }
          out 'and'
        } else if (!conj) {
          out ';'
          out 'but'
        }
      } else if (j > 0) {
        out ","
      }
      generator(cur, j == conjuncts.size() - 1)
    }
  }

  private List<ClauseGenerator> planClauses(Chart chart) {
    List<Frame> allVerbs = chart.frames.findAll { isVerb(it) || it.type in ['fact', 'question'] } as List

    LinkedHashSet<Frame> subordinates = []
    for (head in allVerbs) {
      for (attr in ['elaboration', 'theme', 'message', 'arg2', 'question', 'ifCondition', 'whenCondition', 'perfectBackground', 'reason']) {
        subordinates.addAll(head.f(attr)?.flatten() ?: [])
      }
      subordinates.addAll(head.f('goal')?.f('relative')?.flatten() ?: [])
      subordinates.addAll(head.f('condition')?.f('whenCondition')?.flatten() ?: [])
      subordinates.addAll(head.f('condition')?.f('ifCondition')?.flatten() ?: [])
      if (head.type in ['question', 'fact']) {
        subordinates.addAll(head.f('content')?.flatten() ?: [])
      }
      if (head.type in ['modality', 'BEGIN']) {
        subordinates.addAll(head.f('arg1')?.flatten() ?: [])
      }
    }
    allVerbs.removeAll(subordinates)

/*
    allVerbs.sort({ Frame f1, Frame f2 ->
      chart.earlier(f1, 'type', f2, 'type') ? -1 : f1 == f2 ? 0 : 1
    } as Comparator)
*/

    List<ClauseGenerator> clauses = []
    int i = 0
    int mainVerbs = 0
    while (i < allVerbs.size()) {
      def cur = allVerbs[i++]
      if (cur) {
        mainVerbs++
      }
      Frame comp = null
      def next = allVerbs[i]
      if (cur?.type == 'GO' && next?.type == 'ASK') {
        comp = next
        i++
      }
      clauses << new ClauseGenerator(cur, true, comp)
    }

    List<ClauseGenerator> result = []
    Set<Frame> covered =[]
    for (each in clauses) {
      if (!covered.add(each.frame)) continue

      def seq = each.frame.usages('member')[0]
      if (seq && !each.comp) {
        def parent = new ClauseGenerator(seq, true, null)
        result << parent
        for (child1 in seq.flatten()) {
          covered << child1
          def childGen = new ClauseGenerator(child1, true, null)
          parent.members << childGen
          if (child1.f('member')) {
            for (child2 in child1.flatten()) {
              covered << child2
              childGen.members << new ClauseGenerator(child2, true, null)
            }
          }
        }
      } else {
        result << each
      }
    }
    return result
  }

  private def isVerb(Frame f) {
    f.type in ['HAPPEN', 'FORGET', 'COME_SCALARLY',
            'GO_OFF', 'ASK',
            'DISCOVER', 'REMEMBER',
            'GO', 'SMILE', 'TAKE_OUT', 'SAY', 'MOVE',
            'THANK', 'RUN_OUT',
            'GET_SAD', 'SEEM', 'THINK', 'SIT',
            'modality', 'DO', 'NEED',
            'BEGIN', 'STOP', 'COME_TO',
            'ARGUE', 'FALL', 'BREAK',
            'DISTRACT', 'DISPERSE', 'degree', 'copula'
    ]
  }

  private def emitMoveTail(Frame verb) {
    if (verb.s('manner') == 'SLIGHTLY') {
      out 'slightly'
    }
    out 'back'
    out 'and'
    out 'forth'
  }

  private String opinionOf(Frame opinion, String prevOpinion, Frame parent) {
    if (opinion.f('member')) {
      groovy.lang.Reference prev = [null]
      seq(opinion) {
        String p = prev.get()
        prev.set(opinionOf(it, p, opinion))
      }
      return true
    }

    if (opinion.type == 'OPINION') {
      if (prevOpinion != 'OPINION' || opinion.f('distinguished_in')) {
        out 'in'
      }
      np(opinion, false)
    }
    else if (opinion.type == 'WORDS') {
      out 'according'
      out 'to'
      if (parent) {
        //todo generic right periphery ellipsis
        List<Frame> members = parent.allAssignments('member').collect { it.value } as List
        def i = members.indexOf(opinion)
        if (i < members.size() - 1 && members[i + 1].type == 'OPINION' && members[i + 1].f('arg1') == opinion.f('author')) {
          return opinion.type
        }
      }
      np(opinion.f('author'), false)
    }
    return opinion.type
  }

  private String aux(Frame verb, Frame subj, boolean imperative) {
    def sitPast = past
    if (imperative && subj.hasType()) {
      if (subj.type == 'WE') {
        return sitPast ? 'were': 'are'
      }
      return sitPast ? 'was': 'am'
    }
    if (verb.s('progressive') && !skipAlreadyMentionedArgument(verb, 'progressive')) {
      return subj.type == 'HE' ? 'was' : 'were'
    }
    def questioned = findQuestioned(verb)
    if (questioned && questioned != subj) {
      return sitPast ? 'did' : 'do'
    }

    return null
  }

  Frame findQuestioned(Frame verb) {
    if (!verb) return null
    if (verb.f('arg1')?.type == 'wh') return verb.f('arg1')
    if (verb.f('arg2')?.type == 'wh') return verb.f('arg2')
    if (verb.f('topic')?.type == 'wh') return verb.f('topic')
    if (verb.f('arg2')?.f('arg1')?.type == 'wh') return verb.f('arg2')
    return null
  }

  private Frame clauseInner(Frame verb, boolean shouldHaveSubject, boolean forceSubject, boolean topLevel) {
    assert verb

    def mainFrame = verb
    if (verb.type == 'question') {
      verb = verb.f('content')
    }
    if (verb.s('time') == 'PAST') {
      past = true //todo maintain a tense for the whole discourse
    }
    def imperative = verb.type == 'modality'
    if (imperative) {
      verb = verb.f('arg1')
      shouldHaveSubject = verb.f('arg1')?.hasType()
    }

    def subj = verb.f('arg1')
    if (verb.type == 'degree') {
      out 'great'
      out 'was'
      np(subj, true)
      return subj
    }

    def questioned = findQuestioned(verb)
    if (questioned && !mainFrame.usages('relative')) {
      np(questioned, false, true)
    }

    def opinion = verb.f('opinion_of')
    if (opinion) {
      if (questioned) {
        out ','
      }
      opinionOf(opinion, null, null)
      if (!isCompleteСlauseEllipsis(verb)) {
        out ','
      }
    }

    if (verb.type == 'copula') {
      generateCopula(verb, subj)
      return subj
    }

    def aux = subj?.hasType() ? aux(verb, subj, imperative) : null
    if (topLevel && aux) {
      out aux
    }

    generateSubject(shouldHaveSubject, forceSubject, verb, subj)

    if (aux && !topLevel && aux != 'did') {
      out aux
    }

    if (isCompleteСlauseEllipsis(verb)) {
      out(past ? 'did' : 'does')
      return
    }

    if (verb.s('negated') && verb.type != 'CAN') {
      out 'not'
    }

    if (verb.s('manner') == 'SUDDENLY') {
      out 'suddenly'
    }
    if (verb.s('manner') == 'JUST') {
      out 'just'
    }
    def type = verb.type
    if (type == 'SMILE') {
      out 'gave'
      out 'us'
      out 'a'
      if (verb.s('manner') == 'SADLY') {
        out 'sad'
      }
      out 'smile'
    }
    else if (type == 'COME_SCALARLY') {
      out(verb.s('time') == 'PAST' ? 'went' : 'comes')
      def anchor = verb.f('anchor')
      if (verb.s('order') == 'EARLIER') {
        if (anchor?.type) {
          out 'before'
          np(anchor, false)
        } else {
          out 'first'
        }
      } else if (verb.s('order') == 'AFTER') {
        if (anchor?.type) {
          out 'after'
          np(anchor, false)
        } else {
          out 'next'
        }
      }
    }
    else if (type == 'SEEM') {
      out 'were'
      def theme = verb.f('theme')
      if (theme?.type == 'LACK') {
        out 'void'
        out 'of'
        np(theme.f('arg2'), false)
      } else if (theme?.type == 'MEANINGLESS') {
        out 'meaningless'
      }
    }
    else {
      if (imperative) {
        if (subj?.hasType()) {
          out 'supposed'
        }
        out 'to'
      }

      out mainVerb(verb, imperative || aux && questioned && topLevel)
    }

    Frame addressee = verb.f('addressee')
    if (addressee?.hasType()) {
      np(addressee, false, true)
    }

    def dobj = verb.f('arg2')
    def nounDObj = dobj && dobj.flatten().every { !isVerb(it) && !(it.type in ['fact', 'question']) } && dobj.hasType()
    if (nounDObj && dobj != questioned) {
      np(dobj, false, !isNumber(dobj))
    }

    def mood = verb.f('mood')
    def cheerfully = mood?.type == 'JOY' && !mood.s('size')
    def simpleMood = cheerfully && verb.chart.earlier(mood, 'type', verb, 'type')
    if (simpleMood) {
      out 'cheerfully'
    }

    if (verb.type == 'MOVE') {
      emitMoveTail(verb)
    }

    if (verb.type == 'DISPERSE' && verb.f('goal')?.type == 'HOMES') {
      out 'home'
    } else {
      def io = verb.type == 'HAPPEN' ? verb.f('experiencer') :
        verb.type in ['GO_OFF', 'GO', 'DISPERSE'] ? verb.f('goal') :
          verb.type in ['ASK', 'THINK'] ? verb.f('topic') :
            verb.type in ['DISTRACT'] ? verb.f('theme') :
              verb.type == 'FALL' ? verb.f('source') : null
      if (io?.hasType()) {
        out(verb.type == 'DISTRACT' ? 'from' : verb.type == 'FALL' ? 'off' : verb.type in ['ASK', 'THINK'] ? (io == questioned ? 'about' : 'on') : "to")
        if (io != questioned) {
          np(io, false)
        }
      }
    }


    if (verb.type in ['TAKE_OUT', 'RUN_OUT']) {
      out 'out'
      out 'of'
      np(verb.f('source'), false)
    }

    if (mood && !simpleMood) {
      if (cheerfully) {
        out 'cheerfully'
      } else {
        out 'with'
        np(mood, false)
      }
    }


    if (verb.type == 'HAPPEN') {
      out "today"
    }
    generateTimeAdverbs(verb)

    def questionComp = verb.f('question')
    if (questionComp) {
      generateQuestionComp(questionComp)
    }

    def theme = verb.type == 'FORGET' && !nounDObj ? verb.f('arg2') : verb.f('theme')
    if (theme && verb.type in ['FORGET', 'DISCOVER']) {
      generateComplement(theme)
    } else if (theme && verb.type in ['CAN']) {
      clauseInner(theme, false, false, false)
    } else if (theme && verb.type == 'BEGIN') {
      out(theme.type == 'TO_WATER' ? 'watering' : theme.type == 'DANCE' ? 'dancing' : theme.type == 'ARGUE' ? 'arguing' : 'counting')
      def dObj = theme.f('arg2')
      if (dObj) {
        np(dObj, false)
      }
    }
    return subj
  }

  private void generateCopula(Frame verb, Frame subj) {
    def question = subj.usages('questioned')[0]
    if (verb.f('owner') && !question) {
      np(verb.f('owner'), true)
      out(verb.s('time') == 'PAST' ? 'had' : 'have')
      np(subj, false)
    } else {

      if (question) {
        out(verb.s('time') == 'PAST' ? 'that was' : "that's")
      } else {
        np(subj, true)
        out(subj.type == 'ME' ? "'m" : verb.s('time') == 'PAST' ? 'was' : 'is')
      }
      generateTimeAdverbs(verb)
      out 'on'
      np(verb.f('location'), false)
    }
  }

  private void generateTimeAdverbs(Frame verb) {
    if (verb.s('anchor') == 'AGAIN') {
      out 'again'
    }
    else if (verb.s('anchor') == 'ALREADY') {
      out 'already'
    }

    if (verb.s('duration') == 'LONG') {
      out 'for a long time'
    }
  }

  private void generateQuestionComp(Frame questionComp) {
    def question = questionComp?.f('content')
    if (question?.type == 'THINK') {
      out 'about'
      def exp = question.f('arg1')
      if (exp?.type == 'THEY') {
        out 'their'
      }
      out 'opinion'
      out 'on'
      np(question.f('topic'), false)
      return
    }
    generateComplement(questionComp)
  }

  private void generateComplement(Frame comp) {
    List<ClauseGenerator> clauses = comp.flatten().collect {
      def generator = new ClauseGenerator(it.f('content'), false, null)
      generator.wrapper = it
      generator
    }
    if (clauses[0].wrapper.type == 'fact') {
      out ","
    }
    generateConjuncts(clauses) { conj, last ->
      if (conj.wrapper.type == 'fact') {
        out 'that'
      }
      else if (conj.wrapper.s('negated')) {
        out 'not'
      }
      conj.generate(last)
    }
  }

  boolean isCompleteСlauseEllipsis(Frame verb) {
    return verb.findMeta('type', 'ellipsis') && !hasNonEllidedArguments(verb)
  }

  boolean hasNonEllidedArguments(Frame verb) {
    return verb.type == 'COME_SCALARLY' && !verb.findMeta('order', 'ellipsis')
  }

  private boolean isQuestion(Frame verb) {
    return verb && verb.flatten().any { findQuestioned(it) }
  }

  private def generateSubject(boolean shouldHaveSubject, boolean forceSubject, Frame verb, Frame subj) {
    if (skipAlreadyMentionedArgument(verb, 'arg1')) {
      if (forceSubject) {
        out(subj?.s('gender') == 'fem' ? 'she' : 'he')
      }
      return
    }

    if (!shouldHaveSubject) {
      return
    }

    if (!subj) {
      println "No subject: $verb"
      return
    }
    if (subj.type != 'wh') {
      np(subj, true)
    }
  }


  private def generateQuestionVariants(Frame subj) {
    def variants = subj?.f('variants')
    if (variants) {
      out '-'
      np(variants, true)
    }
  }

  private def generateDirectSpeech(Frame verb) {
    if (verb.type == 'SAY') {
      def message = verb.f('message')
      if (message?.s('directSpeech')) {
        out ':'
        if (message.type) {
          out '\n-'
          capitalize = true
          new ClauseGenerator(message, true, null).generate(true)
        }
      } else if (message) {
        message = message.f('content')
        if (!isQuestion(message)) {
          out 'that'
        } else if (message.f('opinion_of')) {
          out ','
        }
        new ClauseGenerator(message, false, null).generate(true)
      }
    }
  }

  private def generateReason(Frame verb) {
    def reason = verb.f('reason')
    if (reason) {
      out 'because'
      new ClauseGenerator(reason, false, null).generate(true)
    }
  }

  private def generateCondition(Frame verb) {
    def condition = verb.f('condition')
    if (condition) {
      out ','
      out 'only'
      out 'if'
      def content = condition.f('whenCondition') ?: condition.f('ifCondition')
      if (!content) {
        println "No condition: " + condition
        return
      }

      new ClauseGenerator(content, false, null).generate(true)
    }

    def condComp = verb.f('ifCondition') ?: verb.f('whenCondition')
    if (condComp) {
      out ','
      out(verb.f('ifCondition') ? 'if' : 'when')
      new ClauseGenerator(condComp, false, null).generate(true)
    }
  }

  private def generateElaboration(Frame verb) {
    def elaboration = verb.f('elaboration')
    if (!elaboration && verb.type == 'BEGIN') {
      elaboration = verb.f('theme').f('elaboration')
    }

    if (elaboration) {
      def commaStyle = verb.type == 'HAPPEN'
      out(commaStyle ? ',' : ":")
      def sameSentence = commaStyle || verb.type == 'BEGIN'
      List<ClauseGenerator> clauses = elaboration.flatten().collect { new ClauseGenerator(it, !sameSentence, null) }
      generateConjuncts(clauses) { conj, last -> conj.generate(last) }
    }
  }

  private String mainVerb(Frame verb, boolean infinitive) {
    def type = verb.type
    def verbs = [HAPPEN: 'happened', FORGET: 'forgot', GO_OFF: 'went',
            ASK: (verb.f('topic')?.type == 'PREDICAMENT' ? 'consulted' : (past && !infinitive ? 'asked' : 'ask')),
            DISCOVER: 'discovered', CAN: "couldn't", RECALL: 'recall', TAKE_OUT: "took",
            SAY:(verb.f('addressee')?.hasType() ? 'told' : 'said'),
            REMEMBER: (past ? 'remembered' : verb.f('arg1')?.typeInferred in ['HE', 'SHE'] ? 'remembers' : 'remember'), GO: (past ? 'went' : 'goes'),
            RUN_OUT: 'ran', THANK: 'thanked',
            GET_SAD: 'got sad', DO:(past && !infinitive ? 'did' : 'do'), NEED:'need',
            BEGIN:'started', STOP:'stopped',
            MOVE:'moved',
            ARGUE:'were arguing', 'FALL':'fell', BREAK:(past && !infinitive ? 'broke' : 'break'),
            THINK:(verb.s('progressive') ? 'thinking' : 'think'), SIT:(verb.s('progressive') ? 'sitting' : 'sit'),
            DISTRACT:'distracted',
            DISPERSE:'went'
    ]
    return verbs[type] ?: '???'
  }

  def npseq(Frame _seq, boolean mayHaveDeterminer) {
    seq(_seq) { np(it, true, mayHaveDeterminer && !isNumber(it)) }
  }

  def seq(Frame seq, Closure action) {
    def members = seq.flatten()
    members.eachWithIndex { Frame frame, int index ->
      if (index == members.size() - 1) out seq.s('conj')
      else if (index > 0) out ','
      action(frame)
    }
  }

  private def np(Frame n, boolean nom, boolean mayHaveDeterminer = true) {
    if (!n) {
      out '???'
      return
    }

    String type = n.typeInferred

    if (type == 'ME') {
      out (nom ? 'I' : "me")
      return
    }
    if (type == 'HE') {
      out (nom ? 'he' : "him")
      return
    }
    if (type == 'SHE') {
      out (nom ? 'she' : "her")
      return
    }
    if (type == 'WE') {
      out (nom ? 'we' : "us")
      if (n.s('quantifier') == 'ALL') {
        out 'all'
      }
      return
    }
    if (type == 'THEY') {
      out (nom ? 'they' : "them")
      if (n.s('quantifier') == 'ALL') {
        out 'all'
      }
      return
    }

    if (type == 'wh') {
      out(n.s('animate') ? 'who' : 'what')
      return
    }
    if (type == 'THAT') {
      out 'that'
      return
    }

    if (type == 'NEXT') {
      out 'what comes next' //todo a non-hacky model for 'what comes next'
      return
    }

    if (n.findMeta('type', 'ellipsis')) {
      out 'it'
      return
    }

    if (!mayHaveDeterminer && isNumber(n)) {
      out n.type
      return
    }

    if (n.f('member')) { //todo a type for seq frames
      npseq(n, mayHaveDeterminer)
      return
    }

    if (mayHaveDeterminer) {
      determiner(n, n.s('property') == 'AMAZING')
    }
    if (n.s('quality') == 'HUMBLE') {
      out "humble"
    }
    if (n.s('property') == 'AMAZING') {
      out "amazing"
    }
    if (n.s('kind') == 'COMMERCIAL') {
      out "commercial"
    }
    if (n.s('size') == 'LITTLE') {
      out "small"
    }
    if (n.s('size') == 'BIG') {
      out(n.type == 'GARDEN' ? 'big' : "great")
    }
    def criterion = n.f('criterion')
    if (criterion) {
      out noun(criterion)
    }
    if (type == 'CASHIER' && n.f('arg1')?.hasType()) {
      np(n.f('arg1'), false, false)
    }

    if (type == 'SHOP') {
      if (n.s('name') == 'гастроном') {
        out 'grocery'
      }
      out('store')
    } else if (type == 'GARDEN') {
      out(n.s('name') == 'Летний сад' ? 'Summer Garden' : 'garden')
    }
    else if (isNumber(n)) {
      out n.type
    }
    else if (n.s('name') == 'Вася') {
      out 'Vasya'
    }
    else {
      out noun(n)
    }

    def location = n.f('location')
    if (location) {
      out 'on'
      np(location, false)
    }

    if (n.type == 'CORNER') {
      def arg1 = n.f('arg1')
      if (arg1?.hasType()) {
        out 'of'
        seq(arg1) { out it.s('name') == 'знаменская' ? 'Znamenskaya' : 'Basseinaya' }
        out 'streets'
      }
    }
    if (n.type == 'OPINION') {
      def arg1 = n.f('arg1')
      if (arg1 && !isShortPronoun(arg1)) {
        out 'of'
        np(arg1, false)
      }
    }

    def rel = n.f('relative')
    if (rel) {
      out ','
      out 'the one'
      new ClauseGenerator(rel, false, null).generate(false)
    }
  }

  private boolean isShortPronoun(Frame noun) {
    return noun?.typeInferred in ['ME', 'THEY', 'HE', 'SHE', 'WE']
  }

  private boolean isNumber(Frame n) {
    return n.s('number') == 'true' && Util.parseNumber(n.type)
  }

  private void determiner(Frame n, boolean an) {
    if (n.s('name') && n.type == 'MAN') {
      return
    }

    def det = n.s('determiner')
    if (det == 'THIS') {
      out 'this'
      return
    }
    if (det == 'ANY') {
      out 'any'
      return
    }
    if (det == 'SOME') {
      out 'a'
      return
    }

    boolean hasQuantifier = false
    if (n.f('quantifier')?.type == 'BOTH') {
      out 'both'
      hasQuantifier = true
    }
    else if (n.f('quantifier')?.type) {
      def quantifier = n.f('quantifier')
      out(quantifier.s('number') == 'true' ? quantifier.type : noun(quantifier))
      hasQuantifier = true
    }

    if (n.type in ['AMAZE', 'PREDICAMENT', 'NEIGHBOURS', 'MOUTH', 'NOSE', 'JAW', 'JAWS', 'FINGERS', 'ARGUE'] &&
        n.f('arg1')?.resolve()?.hasType() &&
        !skipAlreadyMentionedArgument(n, 'arg1')) {
      if (hasQuantifier) {
        out 'of'
      }
      possessive(n.f('arg1').resolve(), n)
      return
    }
    if (hasQuantifier) {
      return
    }

    if (n.s('name') && !n.type) {
      return
    }

    if (n.type in ['OPINION'] && isShortPronoun(n.f('arg1'))) {
      possessive(n.f('arg1'), n)
      return
    }
    if (n.type == 'WORDS' && n.f('author')) {
      possessive(n.f('author'), n)
      return
    }
    if (n.type in ['ROOMS', 'APARTMENTS', 'OFFICES'] && n.f('owner')) {
      if (!skipAlreadyMentionedArgument(n, 'owner')) {
        possessive(n.f('owner'), n)
      }
      return
    }


    if (n.type in ['JOY', 'RELIEF', 'MEANING', 'MONEY', 'TREES', 'SOME', 'OTHERS']) { //abstract
      return
    }

    if (n.type == 'OPINION' && n.f('arg1')?.type == 'OTHERS') {
      return //a hack
    }

    if (n.type in ['1', '2', '3', '4', '5', '6', '7']) { out 'a'; return }
    if (n.type == '8') { out 'an'; return }

    if (n.s('given') != 'false') {
      out 'the'
      return
    }

    out(an ? "an" : "a")
  }

  private void possessive(Frame poss, Frame noun) {
    if (poss.f('member')) {
      seq(poss) { possessive(it, noun) }
      return
    }

    if (poss.type == 'ME') out 'my'
    else if (poss.type == 'WE') out 'our'
    else if (poss.type == 'THEY') out 'their'
    else if (poss.type == 'HE') out 'his'
    else if (poss.type == 'SHE') out 'her'
    else if (poss.type == 'wh') out 'whose'
    else {
      if (poss.usages('arg1')) {
        out(poss.s('gender') == 'fem' ? 'her' : 'his')
        return
      }

      np(poss, false, false)
      out "'s"
    }
  }

  boolean skipAlreadyMentionedArgument(Frame frame, String attr) {
    Frame arg = frame.f(attr)
    def base = (frame.usages('member')[0] ?: frame).flatten()
    def i = base.indexOf(frame)
    if (i > 0) {
      List<Frame> frames = base[0..<i] //todo alex infer
      if (frames.any { it.f(attr) == arg }) {
        return true
      }
    }
    return false
  }



  private String noun(Frame noun) {
    def type = noun.type
    switch (type) {
      case 'THING': return 'thing'
      case 'NEIGHBOURS': return 'neighbors'
      case 'MATTER': return 'matter'
      case 'AMAZE': return 'amazement'
      case 'COUNTING': return 'counting'
      case 'ORDER': return 'order'
      case 'CASHIER': return 'cashier'
      case 'CORNER': return 'corner'
      case 'PREDICAMENT': return 'predicament'
      case 'HAMMER': return 'hammer'
      case 'MOUTH': return 'mouth'
      case 'NOSE': return 'nose'
      case 'JAW': return 'jaw'
      case 'JAWS': return 'jaws'
      case '1': return 'one'
      case '2': return 'two'
      case '3': return 'three'
      case '4': return 'four'
      case '5': return 'five'
      case '6': return 'six'
      case '7': return 'seven'
      case '8': return 'eight'
      case '9': return 'nine'
      case 'JOY': return 'joy'
      case 'RELIEF': return 'relief'
      case 'WORDS': return 'words'
      case 'MEANING': return 'meaning'
      case 'TREES': return 'trees'
      case 'MONEY': return 'money'
      case 'OPINION': return 'opinion'
      case 'SOME': return 'some'
      case 'OTHERS': return 'others'
      case 'WORK': return 'work'
      case 'CHILD': return 'child'
      case 'BENCH': return 'bench'
      case 'FINGERS': return 'fingers'
      case 'FINGER': return 'finger'
      case 'ARGUE': return 'argument'
      case 'ROOMS': return 'rooms'
      case 'APARTMENTS': return 'apartments'
      case 'OFFICES': return 'offices'
      case 'WATER_MELON': return 'water melon'
      default: return type ?: '???'
    }

  }

}
