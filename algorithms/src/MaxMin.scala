object MaxMin {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val n = inputIter.next().toInt
    val k = inputIter.next().toInt

    val v = inputIter.take(n).map(_.toInt).toVector.sorted

    println((k until n).map(i => v(i - 1) - v(i - k)).min)
  }
}
