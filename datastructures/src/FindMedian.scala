import scala.collection.mutable.ArrayBuffer

object FindMedian {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("datastructures/input.txt").getLines()

    val n = inputIter.next().toInt
    var size = 0

    val a = scala.collection.mutable.ArrayBuffer[Int]()

    inputIter.map(_.toInt).foreach { i =>
      println(insertAndFindNewMedian(i, a, size))
      size += 1
    }
  }

  def insertAndFindNewMedian(i: Int, a: ArrayBuffer[Int], size: Int, j: Int = 0): Float = {
    var j = 0
    while (j < size && a(j) < i && a(j) != -1)
      j += 1

//    var prev = a(j)
//    a(j) = i
//
//    for (k <- j + 1 to size) {
//      val temp = a(k)
//      a(k) = prev
//      prev = temp
//    }

    a.insert(j, i)

    if (size % 2 == 0) {
      a(size / 2)
    }
    else {
      (a(size / 2) + a(size / 2 + 1)).toFloat / 2
    }
  }
}
