object TheFullCountingSort {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val n = inputIter.next().toInt

    val result = inputIter.take(n)
      .map(_.split(" "))
      .map(t => (t.head.toInt, t.last))
      .zipWithIndex
      .foldLeft(Array.fill(100)(new StringBuilder)) { case (s, ((key, w), i)) =>
        if (s(key).nonEmpty)
          s(key).append(" ")

        s(key).append(if (i < n/2) "-" else w)

        s
      }
      .foldLeft(new StringBuilder) { (result, sb) =>
        if (result.nonEmpty)
          result.append(" ")

        result.append(sb)
        result
      }

    println(result.toString())
  }
}
