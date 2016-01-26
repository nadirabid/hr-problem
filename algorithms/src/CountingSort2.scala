object CountingSort2 {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()
    inputIter.next().toInt

    val c = inputIter.next().split(" ").map(_.toInt).foldLeft(Array.fill(100)(0)) { (c, e) =>
      c(e) += 1
      c
    }

    c.zipWithIndex.foreach { case (e, i) =>
      (0 until e).foreach(_ => print(s"$i "))
    }
  }
}
