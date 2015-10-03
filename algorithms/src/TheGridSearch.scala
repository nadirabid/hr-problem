object TheGridSearch {

  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()
    val t = inputIter.next().toInt

    for(ln <- inputIter) {
      val gridRows +: _ +: _ = ln.split(" ").map(_.toInt).toSeq
      val grid = inputIter.take(gridRows).map(_.split("").map(_.toInt).toSeq).toIndexedSeq

      val patternRows +: _ +: _ = inputIter.next().split(" ").map(_.toInt).toSeq
      val pattern = inputIter.take(patternRows).map(_.split("").map(_.toInt).toSeq).toIndexedSeq

      println(if (doesPatterExistInGrid(grid, pattern)) "YES" else "NO")
    }
  }

  def doesPatterExistInGrid(grid: Seq[Seq[Int]], pattern: Seq[Seq[Int]]): Boolean = {
    for (r <- 0 to grid.length - pattern.length)
      for (c <- 0 to grid.head.length - pattern.head.length)
        if (grid(r)(c) == pattern.head.head && testPatternAtGridIndex(grid, pattern, r, c))
          return true

    false
  }

  def testPatternAtGridIndex(grid: Seq[Seq[Int]], pattern: Seq[Seq[Int]], r: Int, c: Int): Boolean = {
    for (m <- pattern.indices)
      for (n <- pattern.head.indices)
        if (pattern(m)(n) != grid(r + m)(c + n))
          return false

    true
  }
}
