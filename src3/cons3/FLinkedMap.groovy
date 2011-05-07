package cons3

import groovypp.concurrent.FList
import java.util.Map.Entry

/**
 * @author peter
 */
final class FLinkedMap<K,V> implements Iterable<Map.Entry<K,V>> {
  private static final FLinkedMap empty = new FLinkedMap(FList.emptyList)
  private final FList<MyEntry<K, V>> list

  static <K, V> FLinkedMap<K, V> getEmptyMap() {
    empty
  }

  private FLinkedMap(FList<MyEntry<K, V>> list) {
    this.list = list
  }

  FLinkedMap<K, V> put(K key, V value) {
    def existing = get(key)
    if (existing == value) return this
    return new FLinkedMap<K,V>(remove(key).list + new MyEntry(key, value))
  }

  FLinkedMap<K, V> remove(K key) {
    MyEntry<K,V> existing = list.find { it.key == key }
    if (existing) {
      return new FLinkedMap<K,V>(list.remove(existing))
    }
    return this
  }

  FLinkedMap<K, V> removeAll(Iterable<K> key) {
    def result = this
    key.each { result = result.remove(key) }
    return result
  }


  FLinkedMap<K, V> plus(Map<K, V> map) {
    groovy.lang.Reference<FLinkedMap<K, V>> ref = new Reference<FLinkedMap<K,V>>(this)
    map.each { k, v -> ref.set(ref.get().put(k, v)) }
    return ref.get()
  }

  V get(K key) {
    MyEntry<K, V> existing = list.find { it.key == key }
    return existing?.value
  }

  V getAt(K key) {
    return get(key)
  }

  V getUnresolvedProperty(K key) {
    return get(key)
  }

  Iterator<? extends Entry<K, V>> iterator() {
    return list.iterator()
  }

  @Override
  String toString() {
    return "{ " + list.collect { it.toString() }.join(", ") + "}"
  }

  List<K> keyList() {
    list.collect { it.key }
  }

  private static class MyEntry<K, V> implements Map.Entry<K, V> {
    final K key
    final V value

    MyEntry(K key, V value) {
      this.key = key
      this.value = value
    }

    V setValue(V v) {
      throw new UnsupportedOperationException("setValue is not implemented")
    }

    @Override
    String toString() {
      return "$key -> $value"
    }


  }

}