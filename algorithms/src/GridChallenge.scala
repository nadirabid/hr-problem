object GridChallenge {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val t = inputIter.next().toInt

    for (n <- inputIter) {
      val m = inputIter.take(n.toInt).toArray

      for (i <- m.indices) {
        m(i) = m(i).sorted
      }

      var isLexicographicallySorted = true
      for (i <- m.indices if isLexicographicallySorted) {
        for (j <- 1 until m.length if isLexicographicallySorted) {
          if (m(j - 1)(i) > m(j)(i)) {
            isLexicographicallySorted = false
          }
        }
      }

      println(if (isLexicographicallySorted) "YES" else "NO")
    }
  }
}
