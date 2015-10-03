object ServiceLane {
  def main(args: Array[String]): Unit = {
    val inputItr = io.Source.fromFile("algorithms/input.txt").getLines()
    inputItr.next()
    val width = inputItr.next().split(" ").map(_.toInt).toIndexedSeq

    inputItr
      .map(_.split(" ").map(_.toInt))
      .foreach { case Array(i, j) =>
        println(width.slice(i, j+1).min)
      }
  }
}
