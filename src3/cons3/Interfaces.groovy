package cons3

/**
 * @author peter
 */
interface Predicate1<T> {
  boolean call(T arg)
}
interface Function2<T1,T2,V> {
  V call(T1 t1, T2 t2)
}
interface Function3<T1,T2,T3,V> {
  V call(T1 t1, T2 t2, T3 t3)
}
interface Function4<T1,T2,T3,T4,V> {
  V call(T1 t1, T2 t2, T3 t3, T4 t4)
}
interface Function1<T1,V> {
  V call(T1 t1)
}