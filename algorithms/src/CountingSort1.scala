object CountingSort1 {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val n = inputIter.next().toInt
    val a = inputIter.next().split(" ").map(_.toInt)

    val c = a.foldLeft(Array.fill(100)(0)) { (c, e) =>
      c(e) += 1
      c
    }

    println(c.mkString(" "))
  }

  def test(args: Array[String]): Unit = {
    val inputIter = io.Source.stdin.getLines()

    val n = inputIter.next().toInt
    val a = inputIter.next().split(" ").map(_.toInt)

    val c = a.foldLeft(Array.fill(100)(0)) { (c, e) =>
      c(e) += 1
      c
    }

    println(c.mkString(" "))
  }
}
