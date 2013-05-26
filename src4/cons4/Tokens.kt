package cons4

import java.util.HashMap

class Tokens {
  val number = counter++
  private var allocated = HashMap<Any, Token>()

  fun get(key: String) = Token("$key$number")

  class object {
    var counter : Int = 0
  }

}

data class Token(val id: String) {
  fun toString() = id
}