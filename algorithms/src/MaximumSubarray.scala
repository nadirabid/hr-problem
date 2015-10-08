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
    var globalMax = a.head
    var contiguousMax, runningContiguousMax = a.head

    for (x <- a.drop(1)) {
      runningContiguousMax = math.max(x, runningContiguousMax + x)
      contiguousMax = math.max(contiguousMax, runningContiguousMax)

      globalMax = math.max(globalMax, math.max(x, x + globalMax))
    }

    Vector[Int](runningContiguousMax, globalMax)
  }
}
