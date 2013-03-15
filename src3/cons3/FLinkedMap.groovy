package cons3

import org.pcollections.Empty
import org.pcollections.PSequence

/**
 * @author peter
 */
final class FLinkedMap<K,V> implements Map<K,V> {
  private static final FLinkedMap empty = new FLinkedMap(Empty.stack())
  private final PSequence<MyEntry<K, V>> list

  static <K, V> FLinkedMap<K, V> getEmptyMap() {
    empty
  }

  private FLinkedMap(PSequence<MyEntry<K, V>> list) {
    this.list = list
  }

  FLinkedMap<K, V> putValue(K key, V value) {
    def newEntry = new MyEntry(key, value)
    def newList = list
    def existing = list.find { it.key == key }
    if (existing) {
      newList -= existing;
    }
    return new FLinkedMap<K,V>(newList + newEntry)
  }

  FLinkedMap<K, V> minus(K key) {
    return removeKey(key)
  }

  FLinkedMap<K, V> removeKey(K key) {
    MyEntry<K,V> existing = list.find { it.key == key }
    if (existing) {
      return new FLinkedMap<K,V>(list - existing)
    }
    return this
  }

  FLinkedMap<K, V> removeAll(Iterable<K> key) {
    Reference<FLinkedMap<K, V>> result = [this]
    key.each { result.set(result.get().removeKey(it)) }
    return result.get()
  }


  FLinkedMap<K, V> plus(Map<K, V> map) {
    Reference<FLinkedMap<K, V>> ref = [this]
    map.each { k, v -> ref.set(ref.get().putValue(k, v)) }
    return ref.get()
  }

  static <K, V> FLinkedMap<K, V> fromMapReverse(Map<K, V> map) {
    return emptyMap + map
  }

  V get(Object key) {
    def entry = list.find { it.key == key }
    return entry?.value
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
    Collections.unmodifiableSet(new LinkedHashSet<K>(list.collect {it.key})) //todo 'as LinkedHashSet' slow: asType
  }

  Collection<V> values() {
    Collections.unmodifiableList(list.collect {it.value} as List)
  }

  Set<Map.Entry<K, V>> entrySet() {
    Collections.unmodifiableSet(new LinkedHashSet<Map.Entry<K, V>>(list))
  }

  V put(K k, V v) {
    throw new UnsupportedOperationException("put is not implemented")
  }

  FLinkedMap<K, V> reverse() {
    return new FLinkedMap(Util.reverse(list))
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