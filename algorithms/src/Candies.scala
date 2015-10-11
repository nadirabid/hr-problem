object Candies {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val n = inputIter.next().trim.toInt

    println(minimumNumberOfCandies(inputIter.take(n).map(_.trim.toInt).toVector))
  }

  def minimumNumberOfCandies(r: Vector[Int], s: Int = 0, prev: Int = 0, total: Long = 0L): Long = {
    if (s >= r.length) {
      total
    }
    else {
      var i = s + 1

      while (i < r.length && r(i - 1) > r(i)) {
        i += 1
      }

      val c = if (r.lift(s - 1).getOrElse(Int.MaxValue) < r(s)) {
        math.max(prev + 1, i - s)
      }
      else {
        i - s
      }

      minimumNumberOfCandies(r, s + 1, c,  total + c)
    }
  }
}
