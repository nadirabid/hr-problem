object CountingSort3 {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val n = inputIter.next().toInt

    val c = inputIter
      .take(n)
      .map(_.split(" "))
      .map(e => (e.head.toInt, e.last))
      .foldLeft(Array.fill(100)(0)) { case (c, (e, s)) =>
        c(e) += 1
        c
      }

    c.foldLeft(0) { case (s, count) =>
      print(s"${s + count} ")
      s + count
    }
  }
}
