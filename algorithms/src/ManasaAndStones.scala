object ManasaAndStones {
  import scala.collection.mutable

  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()
    val t = inputIter.next().toInt

    for (_ <- 0 until t) {
      val n = inputIter.next().trim().toInt
      val a = inputIter.next().trim().toInt
      val b = inputIter.next().trim().toInt

      val m = mutable.Map[Int, mutable.Set[Int]]()

      printAllPossibleSeries(n - 1, a, b, 0, m)
      println()
    }
  }

  def printAllPossibleSeries(n: Int, a: Int, b: Int, s: Int = 0, seen: mutable.Map[Int, mutable.Set[Int]]): Unit = {
    if (n > 0 && !(seen.contains(n) && seen(n).contains(s))) {
      printAllPossibleSeries(n - 1, a, b, s + a, seen)
      printAllPossibleSeries(n - 1, a, b, s + b, seen)
    }
    else if (!seen.contains(n) || !seen(n).contains(s)) {
      seen(n) = mutable.Set()
      seen(n).add(s)

      print(s"$s ")
    }
  }
}
