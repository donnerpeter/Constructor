package cons3

/**
 * @author peter
 */
class EnglishGenerator {
  List<String> output = []

  String generate(Chart chart) {
    new SituationGenerator(chart.situations[0]).sentence()
    return output.join(' ')
  }

  private class SituationGenerator {
    Situation situation

    SituationGenerator(Situation situation) {
      this.situation = situation
    }

    private def sentence() {
      def verb = situation.frames.find { it.type in ['HAPPEN', 'FORGET', 'COME_SCALARLY'] }
      assert verb : situation.presentable()

      def subj = verb.type in ['HAPPEN', 'COME_SCALARLY'] ? verb.f('theme') : verb.f('experiencer')
      np(subj, true)

      if (verb.s('manner') == 'SUDDENLY') {
        output << 'suddenly'
      }
      output << (verb.type == 'HAPPEN' ? "happened" : verb.type == 'FORGET' ? 'forgot' : 'comes first')

      def io = verb.type == 'HAPPEN' ? verb.f('experiencer') : null
      if (io) {
        output << "to"
        np(io, false)
      }

      if (verb.type == 'HAPPEN') {
        output << "today,"
      }

      def theme = verb.f('theme')
      if (theme instanceof Situation) {
        new SituationGenerator(theme).sentence()
      }

      def elaboration = situation.f('elaboration')
      if (elaboration instanceof Situation) {
        new SituationGenerator(elaboration).sentence()
      }

      def variants = subj.allAssignments('variant')
      if (variants) {
        output << '-'
        output << variants.collect { (String)it.value }.join(' or ')
      }
    }

    private def np(Frame n, boolean nom) {
      if (!n) {
        output << '???'
        return
      }

      if (n.s('type') == 'ME') {
        output << (nom ? 'I' : "me")
        return
      }

      if (n == situation.f('questioned')) {
        output << 'what'
        return
      }


      output << "An"
      if (n.s('property') == 'AMAZING') {
        output << "amazing"
      }

      def type = n.type
      if (type == 'THING') {
        output << "thing"
      }
      else {
        output << type
      }
    }

  }

}
