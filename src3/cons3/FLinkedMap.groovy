package cons3

import groovypp.concurrent.FList

/**
 * @author peter
 */
final class FLinkedMap<K,V> implements Map<K,V> {
  private static final FLinkedMap empty = new FLinkedMap(FList.emptyList)
  private final FList<MyEntry<K, V>> list

  static <K, V> FLinkedMap<K, V> getEmptyMap() {
    empty
  }

  private FLinkedMap(FList<MyEntry<K, V>> list) {
    this.list = list
  }

  private FList<MyEntry<K, V>> getList() {
    return list
  }

  FLinkedMap<K, V> with(K key, V value) {
    def existing = get(key)
    if (existing == value) return this
    return new FLinkedMap<K,V>(removeKey(key).getList() + new MyEntry(key, value))
  }

  FLinkedMap<K, V> minus(K key) {
    return removeKey(key)
  }

  FLinkedMap<K, V> removeKey(K key) {
    MyEntry<K,V> existing = list.find { it.key == key }
    if (existing) {
      return new FLinkedMap<K,V>(list.remove(existing))
    }
    return this
  }

  FLinkedMap<K, V> removeAll(Iterable<K> key) {
    def result = this
    key.each { result = result.removeKey(key) }
    return result
  }


  FLinkedMap<K, V> plus(Map<K, V> map) {
    groovy.lang.Reference<FLinkedMap<K, V>> ref = new Reference<FLinkedMap<K,V>>(this)
    map.each { k, v -> ref.set(ref.get().with(k, v)) }
    return ref.get()
  }

  V get(Object key) {
    MyEntry<K, V> existing = list.find { it.key == key }
    return existing?.value
  }

  V getAt(K key) {
    return get(key)
  }

  V getUnresolvedProperty(K key) {
    return get(key)
  }

  @Override
  String toString() {
    return "{ " + list.collect { it.toString() }.join(", ") + "}"
  }

  List<K> keyList() {
    list.collect { it.key }
  }

  int size() {
    list.size
  }

  boolean isEmpty() {
    list.empty
  }

  boolean containsKey(Object o) {
    list.find { it.key == o } != null
  }

  boolean containsValue(Object o) {
    list.find { it.value == o } != null
  }

  V remove(Object o) {
    throw new UnsupportedOperationException("remove is not implemented")
  }

  void putAll(Map<? extends K, ? extends V> map) {
    throw new UnsupportedOperationException("putAll is not implemented")
  }

  void clear() {
    throw new UnsupportedOperationException("clear is not implemented")
  }

  Set<K> keySet() {
    Collections.unmodifiableSet(list.collect {it.key} as Set)
  }

  Collection<V> values() {
    Collections.unmodifiableList(list.collect {it.value} as Set)
  }

  Set<Map.Entry<K, V>> entrySet() {
    Collections.unmodifiableSet(list.collect { it } as Set)
  }

  V put(K k, V v) {
    throw new UnsupportedOperationException("put is not implemented")
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