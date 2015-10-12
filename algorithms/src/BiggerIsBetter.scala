object BiggerIsBetter {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val t = inputIter.next().toInt

    inputIter.take(t).foreach { s =>
      println(nextLexicographicallyBiggerPermutation(s))
    }
  }

  def nextLexicographicallyBiggerPermutation(s: String): String = {
    var found = false
    var i, j = s.length - 1

    while(i >= 0 && !found) {
      j = s.length - 1
      while (j > i && !found) {
        if (s(j) > s(i)) {
          found = true
        }
        else {
          j -= 1
        }
        println(i, j)
      }

      if (!found)
        i -= 1
    }

    if (i >= 0) {
      val prefix = s.substring(0, i) + s(j)
      val suffix = s(i) + s.substring(i + 1, j) + (if (j + 1 < s.length) s.substring(j+1) else "")

      prefix + suffix.sorted.mkString
    }
    else {
      "no answer"
    }
  }
}
