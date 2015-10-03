object CutTheSticks {

  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()
    inputIter.next()

    cutAndPrintSticks(inputIter.next().split(" ").map(_.toInt).toIndexedSeq.sorted)
  }

  def cutAndPrintSticks(sticks: IndexedSeq[Int]): Unit = {
    if (sticks.nonEmpty) {
      val cutSticks = sticks.map(_ - sticks.head)
      println(cutSticks.length)
      cutAndPrintSticks(cutSticks.filter(_ > 0))
    }
  }

}
