object ClosestNumbers {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val n = inputIter.next().toInt
    val v = inputIter.next().split(" ").map(_.toInt).sorted.toVector

    var (diff, pairs) = (Int.MaxValue, List[Int]())

    for (i <- v.length - 1 until 0 by -1) {
      val diffI = math.abs(v(i) - v(i - 1))
      if (diffI < diff) {
        diff = diffI
        pairs = v(i - 1) :: v(i) :: Nil
      }
      else if (diffI == diff) {
        pairs = v(i - 1) :: v(i) :: pairs
      }
    }

    println(pairs.mkString(" "))
  }
}
