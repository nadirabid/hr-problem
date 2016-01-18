object InsertionSortPart2 {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()
    val _ = inputIter.next()
    val a = inputIter.next().split(" ").map(_.toInt)

    insertionSort(a)
  }

  def insertionSort(a: Array[Int]): Unit = {
    for (i <- 1 until a.length) {
      sortElementAt(a, i)
      println(a.mkString(" "))
    }
  }

  def sortElementAt(a: Array[Int], i: Int): Unit = {
    val elementToSort = a(i)

    for (i <- i - 1 to 0 by -1) {
      if (a(i) > elementToSort) {
        a(i + 1) = a(i)
      }
      else {
        a(i + 1) = elementToSort
        return
      }
    }

    a(0) = elementToSort
  }
}
