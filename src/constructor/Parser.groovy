package constructor

import java.beans.NameGenerator
import org.codehaus.groovy.runtime.MetaClassHelper

/**
 * @author peter
 */
class Parser {
  Lexicon lexicon
  //final static Descriptor capitalized = new Descriptor("Capitalized")
  final static Descriptor spaceConstruction = new Descriptor("Space")

  def Parser(lexicon) {
    this.lexicon = lexicon;
  }

  Cloud parse(String input) {
    input = input.replace('\n', ' ').toLowerCase() + " "
    Cloud cloud = new Cloud()
    int pos = 0
    while (pos < input.size()) {
      def current = input.substring(pos)
      def suitable = lexicon.storage.keySet().sort({s1, s2 -> s2.size() - s1.size()}).findAll {current.startsWith(it) }
      if (suitable) {
        def best = suitable[0]
        def builders = lexicon.storage[best]
        cloud.addConstructions(builders.collect { it.build([], cloud)}, pos..pos + best.size())
        pos += best.size()
      } else {
        def space = input.indexOf(' ', pos)
        if (space > pos) {
          def word = input[pos..space-1]
          cloud.addConstructions([new Descriptor(word).build([], cloud)], pos..pos+word.size())
        }
        cloud.addConstructions([spaceConstruction.build([], cloud)], space..space+1)
        pos = space+1
      }
      cloud.updateActive()
    }

    return cloud

  }

}