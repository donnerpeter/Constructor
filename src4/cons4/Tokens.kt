package cons4

import java.util.HashMap

private val numbers = HashMap<Any, Int>()

class Tokens(val eqKey: Any = Any()) {
  val number = numbers.getOrPut(eqKey) { numbers.size() }

  fun get(key: String) = Token("$key$number")

  class object {
    fun resetCounter() {
      numbers.clear()
    }
  }

}

data class Token(val id: String) {
  fun toString() = id
}