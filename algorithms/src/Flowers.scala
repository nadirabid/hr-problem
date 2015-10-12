object Flowers {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val n +: k +: _ = inputIter.next().split(" ").map(_.toInt).toSeq

    var i = 0

    val c = inputIter.next().split(" ")
      .take(n.toInt)
      .map(_.toInt)
      .sortBy(-1 * _)

    val (total, _) = c.foldLeft((0, 0)) { case ((runningTotal, runningAmount), c) =>
      (runningTotal + c * (1 + runningAmount / k), runningAmount + 1)
    }

    println(total)
  }
}
