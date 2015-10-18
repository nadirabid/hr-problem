object CuttingBoards {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    inputIter.next().toInt

    for (ln <- inputIter) {
      val y = inputIter.next().split(" ").map(_.toInt).toVector.sortBy(- _)
      val x = inputIter.next().split(" ").map(_.toInt).toVector.sortBy(- _)

      val m = math.pow(10, 9).toInt + 7
      println(minimumCostOfCuttingBoard(y, x) % m)
    }
  }

  def minimumCostOfCuttingBoard(y: Vector[Int], x: Vector[Int], numOfYCuts: Long = 1, numOfXCuts: Long = 1, totalCost: Long = 0): Long = {
    (y, x) match {
      case (yMax +: yTail, xMax +: xTail) =>
        if (xMax > yMax) {
          minimumCostOfCuttingBoard(y, xTail, numOfYCuts, numOfXCuts + 1, totalCost + xMax * numOfYCuts)
        }
        else {
          minimumCostOfCuttingBoard(yTail, x, numOfYCuts + 1, numOfXCuts, totalCost + yMax * numOfXCuts)
        }
      case (yMax +: yTail, Vector()) =>
        minimumCostOfCuttingBoard(yTail, x, numOfYCuts + 1, numOfXCuts, totalCost + yMax * numOfXCuts)
      case (Vector(), xMax +: xTail) =>
        minimumCostOfCuttingBoard(y, xTail, numOfYCuts, numOfXCuts + 1, totalCost + xMax * numOfYCuts)
      case (Vector(), Vector()) =>
        totalCost
    }
  }
}
