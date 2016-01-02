object StockMaximize {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val t = inputIter.next().toInt

    for (n <- inputIter.map(_.toInt)) {
      val pricesByDay = inputIter.next().split(" ").take(n).map(_.toInt)
      println(maximizeProfits(pricesByDay))
    }
  }

  def maximizeProfits(pricesPerDay: Array[Int]): Int = {
    pricesPerDay.indices.foldLeft((0, 0, 0)) { case ((profit, total, remaining), i) =>


      (profit, total, remaining)
    }._1
  }
}
