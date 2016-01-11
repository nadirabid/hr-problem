object TheGridSearch2 {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val t = inputIter.next().toInt

    for (_ <- 0 until t) {
      val r :: _ = inputIter.next().trim.split(" ").map(_.toInt).toList
      val g = inputIter.take(r).map(_.split("").map(_.toInt).toVector).toVector

      val j :: _ = inputIter.next().trim.split(" ").map(_.toInt).toList
      val p = inputIter.take(j).map(_.split("").map(_.toInt).toVector).toVector

      println(if (gridContainsPattern(g, p)) "YES" else "NO")
    }
  }

  def gridContainsPattern(grid: Vector[Vector[Int]], pattern: Vector[Vector[Int]]): Boolean = {
    for (j <- 0 to (grid.length - pattern.length))
      for (i <- 0 to (grid.head.length - pattern.head.length))
        if (patternExistsInGridAt(grid, pattern, j, i))
          return true

    false
  }

  def patternExistsInGridAt(grid: Vector[Vector[Int]], pattern: Vector[Vector[Int]], j: Int, i: Int): Boolean = {
    for (y <- pattern.indices)
      for (x <- pattern.head.indices)
        if (grid(y + j)(x + i) != pattern(y)(x))
          return false

    true
  }
}
