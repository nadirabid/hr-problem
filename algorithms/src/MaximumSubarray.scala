object MaximumSubarray {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val t = inputIter.next().trim.toInt

    for (n <- inputIter) {
      val a = inputIter.next().split(" ").map(_.toInt).toVector
      println(maximumSubarray(a).mkString(" "))
    }
  }

  def maximumSubarray(a: Vector[Int]): Vector[Int] = {
    var max = a.head
    var runningContiguousMax, contiguousMax = a.head

    for (x <- a.drop(1)) {
      runningContiguousMax = math.max(x, x + runningContiguousMax)
      contiguousMax = math.max(contiguousMax, runningContiguousMax)

      max = math.max(max, math.max(x, x + max))
    }

    Vector[Int](contiguousMax, max)
  }
}
