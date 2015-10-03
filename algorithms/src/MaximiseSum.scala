object MaximiseSum {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val t = inputIter.next().toInt

    for (ln <- inputIter) {
      val m = ln.split(" ")(1).toLong
      val arr = inputIter.next().split(" ").map(_.toLong).toIndexedSeq

      println(maxModSumOfSubArrays(arr, m))
      println((1 to arr.length).flatMap(arr.sliding).map(_.sum % m).max)
    }
  }

  def maxModSumOfSubArrays(arr: IndexedSeq[Long], m: Long): Long = {
    var max = 0L

    val previous = scala.collection.mutable.IndexedSeq.fill(arr.length)(0L)

    for (i <- 1 to arr.length) {
      for (j <- i to arr.length) {
        val sum = (previous(j-i) + arr(j - 1)) % m
        previous(j - i) = sum
        //println(sum, arr.view(j - i, j).sum % m)
        max = math.max(sum, max)
      }
    }

    max
  }

  def maxModSumOfSubArrays2(arr: IndexedSeq[Int], m: Int): Int = {
    var max = 0

    for (i <- 1 to arr.length) {
      for (j <- i to arr.length) {
        max = math.max(arr.view(j - i, j).sum % m, max)
      }
    }

    max
  }
}
