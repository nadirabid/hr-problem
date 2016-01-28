object BeadOrnaments {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val t = inputIter.next().toInt

    for (n <- inputIter.take(t).map(_.toInt)) {
      println(numOfOrnamentsPossible(n, inputIter.next().split(" ").map(_.toInt).toVector))
    }
  }

  def numOfOrnamentsPossible(n: Int, b: Vector[Int]): Long = {
    val r = b.foldLeft(BigInt(1))((r, x) => r * math.pow(x, x - 1).toLong) *  math.pow(b.sum, n - 2).toLong

    (r % 1000000007).toLong
  }
}
