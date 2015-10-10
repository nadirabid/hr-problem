object GridWalking {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val t = inputIter.next().toInt

    for (ln <- inputIter) {
      val n +: m +: _ = inputIter.next().split(" ").map(_.toInt).toSeq

      val x = inputIter.take(n).map(_.toInt).toVector
      val d = inputIter.take(n).map(_.toInt).toVector

      println(maxNumberOfSteps(n, m, x, d))
    }
  }

  def maxNumberOfSteps(n: Int, m: Int, x: Vector[Int], d: Vector[Int]): Unit = {

  }

  def findWaysOfMakingMoves(w: Array[Array[Int]]): Unit = {
    for (d <- w) {
      for (m <- d) {

      }
    }
  }

}
