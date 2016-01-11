object CavityMap {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val n = inputIter.next().toInt
    val g = inputIter.take(n).map(_.split("").map(_.toInt).toVector).toVector

    for (j <- 0 until n) {
      for (i <- 0 until n) {
        if (isCellACavity(g, n, j, i)) {
          print("X")
        }
        else {
          print(g(j)(i))
        }
      }
      println()
    }
  }

  def isCellACavity(grid: Vector[Vector[Int]], n: Int, j: Int, i: Int): Boolean = {
    if (j == 0 || i == 0) {
      false
    }
    else if (i == n - 1 || j == n -1) {
      false
    }
    else if (grid(j)(i) <= grid(j - 1)(i)) {
      false
    }
    else if(grid(j)(i) <= grid(j)(i + 1)) {
      false
    }
    else if(grid(j)(i) <= grid(j + 1)(i)) {
      false
    }
    else if(grid(j)(i) <= grid(j)(i - 1)) {
      false
    }
    else {
      true
    }
  }
}
