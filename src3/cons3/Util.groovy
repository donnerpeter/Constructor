package cons3

import groovy.transform.CompileStatic
import org.pcollections.Empty
import org.pcollections.PStack

import static cons3.Construction.cxt
import static cons3.RussianConstructions.colon
import static cons3.RussianConstructions.posleGen
import static cons3.RussianConstructions.preposition
import static cons3.RussianConstructions.ransheGen
/**
 * @author peter
 */
class Util {

  static Integer parseNumber(String word) {
    if (word == null || word.length() == 0 || !Character.isDigit(word.charAt(0))) {
      return null
    }

    try {
      return Integer.parseInt(word)
    } catch (NumberFormatException ignore) {
      return null
    }
  }

  static <T> boolean intersects(Collection<T> set1, Collection<T> set2) {
    if (set2.size() < set1.size()) {
      def temp = set1; set1 = set2; set2 = temp;
    }
    if (!(set2 instanceof Set)) {
      set2 = new HashSet<T>(set2)
    }
    for (x in set1) {
      if (x in set2) {
        return true
      }
    }
    return false
  }

  static <T> LinkedHashSet<T> intersect(Collection<T> set1, Collection<T> set2) {
    def ts = new LinkedHashSet<T>(set1)
    ts.retainAll(new HashSet<T>(set2))
    return ts
  }

  static <T> LinkedHashSet<T> minus(Collection<T> set1, Collection<T> set2) {
    def ts = new LinkedHashSet<T>(set1)
    ts.removeAll(set2)
    return ts
  }

  static Construction commaSurrounded(Construction cxt) {
    cxt.
            enrichingMites { mite, contribution, state -> handleCommaEnrichments(mite, contribution, state) }.
            structural { mite, state, via, up -> getPrevStateForCommaSurrounded(mite, state)
    }
  }

  static List<ParsingState> getPrevStateForCommaSurrounded(Mite mite, ParsingState state) {
    if (mite.contents.comma1 && !mite.contents.content) {
      return [ParsingState.EMPTY]
    }
    if (mite.contents.content) {
      def firstAtom = mite.firstAtom
      if (firstAtom.contents.comma1) {
        def beforeComma = state.findState(firstAtom).prevState?.hierarchy
        if (beforeComma) {
          return beforeComma
        }
      }
    }
    return null
  }

  static List<Mite> handleCommaEnrichments(Mite mite, List<Mite> contribution, ParsingState state) {
    boolean wantContent = mite.contents.comma1 && !mite.contents.content
    boolean wantComma2 = mite.contents.content && !mite.contents.comma2
    if (wantComma2 && mite.contents.comma1 && contribution.find { it.cxt == colon }) {
      def firstAtom = mite.firstAtom
      if (firstAtom?.contents?.comma1) {
        def beforeState = state.findState(firstAtom).prevState
        if (beforeState) {
          return beforeState.enrichUpdate(contribution, state) as List
        }
      }
    }

    return mite.unifyWherePossible(contribution.findAll {
      it.cxt == mite.cxt &&
      (wantContent && it.contents.content ||
       wantComma2 && it.contents.comma2)
    })
  }

  static Construction commonCase(String name) {
    cxt(name, ['noun', 'head']) { ParsingState state, Map args ->
      state
    }.structural { mite, state, via, up ->
      return showCommonCaseHierarchy(up, state, mite)
    }.enrichingMites { mite, contribution, state ->
      if (contribution.find { it.cxt == preposition || it.cxt in [ransheGen, posleGen] && it.contents.noun && !it.contents.head }) {
        return []
      }
      mite.unifyWherePossible(contribution)
    }.satisfiedWhen { it.v('noun')?.hard && it.contents.head }
  }

  static Collection<ParsingState> showCommonCaseHierarchy(boolean up, ParsingState state, Mite mite) {
    if (up) {
      def headState = state.findState(mite, 'head')
      if (headState) {
        if (headState.ownMites.find { it.cxt == preposition }) {
          return [headState] + headState.prevState.hierarchy
        }
        return headState.hierarchy
      }
      return null
    }
    return state.findState(mite.firstAtom)?.prevVisibleState?.hierarchy
  }

  static Construction emptyCxt(String name) {
    cxt(name, ['X']) { state, args -> state }
  }
  static Construction commonPrep(String prep, Construction caze) {
    cxt(prep + caze.name.capitalize(), ['noun', 'head']) { ParsingState state, Map args ->
      state
    }.structural { mite, state, via, up ->
      def parentState = state.findState(mite, 'head')
      return up ? parentState?.hierarchy : state.findState(mite.firstAtom)?.prevState?.hierarchy
    }.satisfiedWhen { it.v('noun')?.hard && it.contents.head }
  }

  static Construction commonSimpleArg(String name, String rel) {
    cxt(name + rel.capitalize(), ['noun', 'head']) { ParsingState state, Map args ->
      state.assign(args.head, rel, args.noun)
    }
  }

  static <T> PStack<T> reverse(Iterable<T> list) {
    PStack<T> result = Empty.stack()
    for (e in list) {
      result += e
    }
    return result
  }
}
