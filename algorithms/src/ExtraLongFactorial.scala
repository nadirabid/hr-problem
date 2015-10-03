object ExtraLongFactorial {
  def main(args: Array[String]): Unit = {
    val n = io.Source.fromFile("algorithms/input.txt").getLines().next().toInt
    println((1 to n).foldLeft(BigInt(1))(_ * _))
  }
}
