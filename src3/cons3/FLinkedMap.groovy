package cons3

import org.pcollections.Empty
import org.pcollections.PMap
import org.pcollections.PSequence

/**
 * @author peter
 */

final class FLinkedMap<K,V> implements Map<K,V> {
  private static final FLinkedMap empty = new FLinkedMap(Empty.stack(), Empty.map())
  private final PSequence<K> list
  @Delegate(interfaces = false)
  private final PMap<K, V> map

  static <K, V> FLinkedMap<K, V> getEmptyMap() {
    empty
  }

  private FLinkedMap(PSequence<K> list, PMap<K, V> map) {
    this.list = list
    this.map = map
    assert map != null
  }

  FLinkedMap<K, V> putValue(K key, V value) {
    def newList = map.containsKey(key) ? list : list.plus(key)
    return new FLinkedMap<K,V>(newList, map.plus(key, value))
  }

  FLinkedMap<K, V> minus(K key) {
    return removeKey(key)
  }

  FLinkedMap<K, V> removeKey(K key) {
    return new FLinkedMap<K, V>(list - key, map - key)
  }

  FLinkedMap<K, V> plus(Map<K, V> map) {
    FLinkedMap<K, V> result = this
    map.each { k, v -> result = result.putValue(k, v) }
    return result
  }

  static <K, V> FLinkedMap<K, V> fromMapReverse(Map<K, V> map) {
    return emptyMap + map
  }

  Set<K> keySet() {
    Collections.unmodifiableSet(new LinkedHashSet<K>(list))
  }

  Collection<V> values() {
    PMap<K, V> _map = map
    Collections.unmodifiableList(list.collect { _map.get(it) } as List) //todo bug
  }

  Set<Map.Entry<K, V>> entrySet() {
    Map<K,V> ordered = new LinkedHashMap<K,V>()
    for (key in keySet()) {
      ordered[key] = map.get(key)
    }
    Collections.unmodifiableSet(ordered.entrySet())
  }

  FLinkedMap<K, V> reverse() {
    return new FLinkedMap(Util.reverse(list), map)
  }

}