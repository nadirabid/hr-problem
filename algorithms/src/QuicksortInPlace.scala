object QuicksortInPlace {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val n = inputIter.next().toInt

    val a = inputIter.next().split(" ").map(_.toInt)
    a.foreach(e => print(s"$e "))
    println()
    quickSort(a, 0, a.length - 1)
  }

  def quickSort(a: Array[Int], lo: Int, hi: Int): Unit = {
    if (lo < hi) {
      val p = partition(a, lo, hi)
      a.foreach(e => print(s"$e "))
      println()

      //quickSort(a, lo, p - 1)
      //quickSort(a, p + 1, hi)
    }
  }

  def partition(a: Array[Int], lo: Int, hi: Int): Int = {
    val pivot = a(hi)

    var i = lo

    for (j <- lo until hi) {
      if (a(j) <= pivot) {
        val temp = a(i)
        a(i) = a(j)
        a(j) = temp
        i += 1
      }
    }

    val temp = a(i)
    a(i) = a(hi)
    a(hi) = temp

    println(i)
    i
  }
}
