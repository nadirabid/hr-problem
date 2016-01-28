object Clique {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val t = inputIter.next().trim.toInt

    inputIter
      .take(t)
      .map(_.split(" ").map(_.toInt))
      .map { case Array(n, m) =>
        minimumSizeOfLargestClique(n, m)
      }
      .foreach(println)
  }

  def minimumSizeOfLargestClique(n:Int, m: Int): Int = {
    for (x <- n until 0 by -1) {
      if (minimumNumberOfEdgesForGraphWithCliqueOfSize(n, x - 1) <= m) {
        return x
      }
    }

    n
  }

  def search(n: Int, m: Int, minI: Int, maxI: Int): Int = {

    0
  }

  def minimumNumberOfEdgesForGraphWithCliqueOfSize(n: Int, r: Int): Double = {
    math.floor(((r - 1.0)/r) * (n*n/2.0)) + 1
  }
}
