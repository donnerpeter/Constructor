package cons3

import groovy.transform.Immutable

/**
 * @author peter
 */
@Immutable class Update {
  Map<Construction, Map<String, Object>> map

  Update addCxt(Map newArgs, Construction name, Closure init = null) {
    if (init) {
      newArgs += [init:init]
    }
    return new Update(map + [(name):newArgs])
  }
}
