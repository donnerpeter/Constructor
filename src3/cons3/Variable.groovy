package cons3

/**
 * @author peter
 */

class Variable {
  static int counter = 0

  final String id
  final Variable base
  private final Variable lightVar

  Variable() {
    id = 'V' + counter++
    base = this
    lightVar = new Variable(this)
  }

  Variable(String comment) {
    id = "Var${counter++}[$comment]"
    base = this
    lightVar = new Variable(this)
  }

  private Variable(Variable base) {
    id = base.id + "_light"
    this.base = base
    lightVar = null
  }

  Variable getLightVar() {
    assert !light
    lightVar
  }

  boolean isLight() {
    return base != this
  }

  boolean isHard() {
    return !isLight()
  }

  String toString() {
    return "$id"
  }

  Frame frame(Chart chart) {
    new Frame(this, chart)
  }

}

