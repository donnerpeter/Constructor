package cons4

object Util {
  fun parseNumber(word: String?): Int? {
    if (word == null || word.length() == 0 || !Character.isDigit(word.charAt(0))) {
      return null
    }

    try {
      return Integer.parseInt(word)
    } catch (ignore: NumberFormatException) {
      return null
    }
  }

}



