object InsertionSortPart1 {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()
    val n = inputIter.next().toInt
    val a = inputIter.next().split(" ").map(_.toInt)

    sortLastElement(a)
  }

  def sortLastElement(a: Array[Int]): Unit = {
    val lastElement = a.last

    for (i <- a.length - 2 to 0 by -1) {
      if (a(i) > lastElement) {
        a(i + 1) = a(i)
        println(a.mkString(" "))
      }
      else {
        a(i + 1) = lastElement
        println(a.mkString(" "))
        return
      }
    }

    a(0) = lastElement
    println(a.mkString(" "))
  }
}
