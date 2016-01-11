object CaesarCypher {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()
    val _ = inputIter.next().toInt
    val s = inputIter.next().toCharArray
    val k = inputIter.next().toInt

    println(encryptString(s, k))
  }

  def encryptString(s: Array[Char], k: Int): String = {
    s.map { c =>
      if (c >= 'A' && c <= 'Z') {
        val letterOffset = ((c - 'A') + k) % 26
        (letterOffset + 'A').toChar
      }
      else if (c >= 'a' && c <= 'z') {
        val letterOffset = ((c - 'a') + k) % 26
        (letterOffset + 'a').toChar
      }
      else {
        c
      }
    }.mkString
  }
}
