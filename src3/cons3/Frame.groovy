package cons3

/**
 * @author peter
 */
class Frame {
  final Variable var
  final Variable unifiedVar
  final Chart chart

  Frame(Variable var, Chart chart) {
    this.var = var
    this.chart = chart
    unifiedVar = chart.getUnifiedVar(var)
  }

  String toString() {
    return "$var: ${chart.allAssignments.findAll { it.frame == (Frame) this }}"
  }

  private Frame findController() {
    def group = usages('member')[0] ?: this
    def clause = group.usages('content')[0]
    if (!clause || !(clause.type in ['fact', 'question'])) {
      return null
    }

    def controller = clause.findClauseController()
    assert controller != this
    return controller
  }

  Frame findClauseController() {
    def group = usages('member')[0] ?: this
    def controller = group.usages('arg2')[0]
    controller = controller ?: group.usages('theme')[0]
    controller = controller ?: group.usages('message')[0]
    controller = controller ?: group.usages('question')[0]
    return controller
  }

  Frame f(String attr, boolean infer = false) {
    def v = definedAttributeValue(attr)
    if (v instanceof Frame) {
      if (attr == 'arg1' && !v.definedAttributeValue('type')) {
        def fromController = findController()?.f('arg1')
        if (fromController && mayReference(v, fromController)) {
          return fromController
        }
      }

      return (Frame) v
    }

    if (attr == 'arg1') {
      if (type == 'NEIGHBOURS') {
        def verb = usages('arg2')[0] ?: usages('goal')[0]
        return verb?.f('arg1')
      }
      if (type in ['MOUTH', 'NOSE', 'JAW', 'JAWS']) {
        def verb = chart.frames.find { it.definedAttributeValue('arg1') }
        if (verb) {
          return (Frame) verb.definedAttributeValue('arg1')
        }
      }

      def controller = findController()
      if (controller) {
        return controller.f('arg1')
      }
    }

    return null
  }

  private static boolean mayReference(Frame ref, Frame target) {
    if (ref.s('person') == '3' && target.type == 'ME') return false
    return true
  }

  boolean definitelyNonHuman() {
    type && !canBeHuman()
  }

  boolean canBeHuman() {
    if (f('member')) {
      return flatten().any { it.canBeHuman() }
    }
    if (type == 'wh') return false
    if (type in ['SHOP']) return false
    if (s('number') || Util.parseNumber(type) != null) return false
    if (!type && s('name')) return true
    return true
  }

  def definedAttributeValue(String attr) {
    def assignments = allAssignments
    if (!assignments) return null

    for (int i in assignments.size() - 1..0) {
      if (assignments[i].property == attr) {
        return assignments[i].value
      }
    }
    return null
  }

  List<Frame> flatten() {
    if (f('member')) {
      LinkedHashSet<Frame> heads = []
      for (member in allAssignments('member')) {
        heads << (Frame) member.value
      }
      assert !(this in heads)
      return heads as List
    }
    return [this]
  }

  String s(String attr, boolean infer = false) {
    def v = definedAttributeValue(attr)
    if (v instanceof String) {
      return (String) v
    }

    if (attr == 'given') {

      if (type in ['THING', 'HAMMER', 'BENCH', 'FINGER', 'JAW', 'WATER_MELON']) return 'false'
      if (type == 'CASHIER') {
        def shop = chart.frames.find { it.type == 'SHOP' }
        return shop && chart.earlier(shop, 'type', this, 'type') ? 'false' : 'true'
      }
      if (type == 'SHOP') {
        if (usages('arg1')) {
          return 'true'
        }
        def cashier = chart.frames.find { it.type == 'CASHIER' }
        if (cashier && chart.earlier(cashier, 'type', this, 'type')) {
          return 'true'
        }

        def verb = chart.frames.find { it.f('source') == (Frame) this }
        return verb ? 'true' : 'false'
      }
      return 'true'
    }

    if (attr == 'type') {
      if (f('member')) return null
      if (s('rusNumber') == 'pl') return 'THEY'
      if (s('name') == 'Вася') return 'MAN'
      if (s('person') == '3' && infer) {
        if (usages('arg1').find { it.humanAction }) {
          return s('gender') == 'fem' ? 'SHE' : 'HE'
        }
      }
    }

    if (attr == 'quantifier' && type in ['WE', 'THEY'] && usages('arg1')[0]?.type == 'DISPERSE') {
      return 'ALL'
    }

    if (attr == 'progressive' && type in ['THINK', 'SIT']) {
      if (findController()?.s('time') == 'PAST') {
        return 'true'
      }
    }

    return null
  }

  String getTypeInferred() { s('type', true) }
  String getType() { s('type') }

  private List<Assignment<Frame>> assignmentCache = null
  List<Assignment<Frame>> getAllAssignments() {
    if (assignmentCache != null) return assignmentCache
    return assignmentCache = chart.allAssignments.findAll { it.frame.unifiedVar == unifiedVar }
  }

  List<Assignment<Frame>> allAssignments(String attr) {
    return allAssignments.findAll { it.property == attr }
  }

  boolean equals(o) {
    if (this.is(o)) return true;
    if (!(o instanceof Frame)) return false;

    Frame frame = (Frame) o;

    if (chart != frame.chart) return false;
    if (unifiedVar != frame.unifiedVar) return false;

    return true;
  }

  int hashCode() {
    int result;
    result = unifiedVar.hashCode();
    result = 31 * result + chart.hashCode();
    return result;
  }

  Frame findMeta(String attr, String metaType) {
    return allMetas(attr).find { it.type == metaType }
  }

  List<Frame> allMetas(String attr) {
    def a = chart.activeAssignments.findAll { chart.getUnifiedVar(it.frame) == unifiedVar && it.property == attr } as List
    Assignment<Variable> last = a.reverse()[0]
    if (last) {
      return chart.__metas[last].findAll { it instanceof Variable }.collect { ((Variable) it).frame(chart) }
    }

    return []
  }

  List<Frame> usages(String attrName) {
    if (attrName == 'member') {
      return chart.frames.findAll { ((Frame) this) in it.allAssignments('member').collect { it.value } }
    }

    chart.frames.findAll { it.f(attrName) == (Frame) this }
  }

  boolean isHumanAction() {
    type in ['GO', 'BREAK', 'CAN', 'REMEMBER', 'FORGET']
  }

  boolean hasType() {
    return flatten()[0].type
  }

  Frame resolve() {
    if (!hasType()) {
      def master = usages('arg1')[0]
      if (master?.type == 'JAW') {
        def verb = chart.frames.find { it.definedAttributeValue('arg1') }
        if (verb) {
          return (Frame) verb.definedAttributeValue('arg1')
        }
      }
    }
    return this
  }
}