package cons3

import groovy.transform.TupleConstructor

/**
 * @author peter
 */
@TupleConstructor
class Contribution {
  final ParsingState before
  final FLinkedMap<Construction, Map> apps
  final Construction satisfied
  final Construction inhibited

  @Override
  String toString() {
    return "$apps"
  }


}
