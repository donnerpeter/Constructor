package cons3

/**
 * @author peter
 */
class Vars {
  private final Map<Object, Variable> allocated = [:]

  public Variable getAt(Object name) {
    def var = allocated.get(name)
    if (!var) {
      allocated.put(name, var = new Variable())
    }
    return var
  }

}
