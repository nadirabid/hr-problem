object DiagonalDifference {
  def main(args: Array[String]): Unit = {
    val inputItr = io.Source.fromFile("algorithms/input.txt").getLines()
    val m = inputItr.next().toInt

    var indexOfDownDiagonal = 0
    var indexOfUpDiagonal = m-1

    var sumOfDownDiagonal = 0
    var sumOfUpDiagonal = 0

    for (ln <- inputItr.take(m)) {
      val row = ln.split(" ")

      sumOfDownDiagonal += row(indexOfDownDiagonal).toInt
      sumOfUpDiagonal += row(indexOfUpDiagonal).toInt

      indexOfDownDiagonal += 1
      indexOfUpDiagonal -= 1
    }

    println(Math.abs(sumOfUpDiagonal - sumOfDownDiagonal))
  }
}
