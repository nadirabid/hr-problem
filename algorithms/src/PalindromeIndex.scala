object PalindromeIndex {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val t = inputIter.next().toInt

    for (ln <- inputIter.take(t)) {
      println(palindromeIndex(ln, 0, ln.length - 1))
    }
  }

  def palindromeIndex(s: String, i: Int, j: Int, adjusted: Boolean = false): Int = {
    if (j < i) {
      -1
    }
    else if (s(i) == s(j)) {
      palindromeIndex(s, i + 1, j - 1, adjusted)
    }
    else if (adjusted) {
      -2
    }
    else {
      if (palindromeIndex(s, i + 1, j, true) == -2) j else i
    }
  }
}
