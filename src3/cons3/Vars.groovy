package cons3

/**
 * @author peter
 */
class Vars {
  private final Map<Object, Variable> allocated = [:]

  Variable getProperty(String name) { getVar(name) }

  Variable getAt(Object name) {
    return getVar(name)
  }

  private Variable getVar(name) {
    def var = allocated.get(name)
    if (!var) {
      allocated.put(name, var = new Variable())
    }
    return var
  }

}
