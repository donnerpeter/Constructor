package cons4

import cons4.constructions.*
import java.util.StringTokenizer

public class Parser {

  fun parse(text : String) : ParsingState {
    var state = ParsingState()
    val tokenizer = StringTokenizer(text, " '\":,.?!", true)
    for (_w in tokenizer) {
      val w = _w as String
      if (w != " ") {
        val logged = state.appendLog("$w ${'-' * (100 - w.length())}\n")
        try {
          state = handleWord(w.toLowerCase().trim(), logged)
        } catch(e: Throwable) {
          state.printLog()
          throw e
        }
      }
    }

    return state
  }

  fun handleWord(w: String, state: ParsingState) : ParsingState {
    return state.apply(word(word=w), word(word="2"))
  }



}