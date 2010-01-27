package constructor.util

/**
 * @author peter
 */
final class Pair<T,V> {
  T fst
  V snd

  def Pair(fst, snd) {
    this.fst = fst;
    this.snd = snd;
  }
}
