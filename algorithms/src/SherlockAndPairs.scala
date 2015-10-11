object SherlockAndPairs {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    inputIter.next()

    for (n <- inputIter) {
      val a = inputIter.next().split(" ").take(n.toInt).map(_.toInt).toVector

      val pairs = scala.collection.mutable.Map[Int, Int]()

      for (i <- a) {
        pairs(i) = pairs.getOrElse(i, 0) + 1
      }

      println(pairs.foldLeft(0L) { case (count, (k, v)) =>
        if (v > 1)
          count + (v.toLong * (v - 1).toLong)
        else
          count
      })
    }
  }
}
