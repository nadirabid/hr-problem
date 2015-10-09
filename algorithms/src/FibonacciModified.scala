object FibonacciModified {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val a +: b +: n +: _ = inputIter.next().trim.split(" ").map(_.toInt).toIndexedSeq
    println(pseudoFibonacci(a, b, n - 2))
  }

  def pseudoFibonacci(a: BigInt, b: BigInt, n: Int): BigInt = {
    if (n == 0)
      b
    else
      pseudoFibonacci(b, b*b + a, n - 1)
  }
}
