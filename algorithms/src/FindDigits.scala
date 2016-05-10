object FindDigits {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val t = inputIter.next().toInt

    for (n <- inputIter.take(t).map(_.toInt)) {
      var count = 0
      var remaining = n

      while (remaining > 0) {
        val next = remaining % 10

        if (next > 0 && n % next == 0)
          count += 1

        remaining /= 10
      }

      println(count)
    }
  }
}
