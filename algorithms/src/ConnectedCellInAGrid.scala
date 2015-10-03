object ConnectedCellInAGrid {
  case class Node(value: Int, var visited: Boolean = false)

  def main(arg: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val rows = inputIter.next().toInt
    val cols = inputIter.next().toInt

    val arr = inputIter.map(_.split(" ").map(n => Node(n.toInt)).toVector).toVector


    println((0 until rows).foldLeft(0) { (rm, r) =>
      math.max(rm, (0 until cols).foldLeft(rm) { (cm, c) =>
        math.max(cm, sizeOfLargestRegion(arr, r, c))
      })
    })
  }

  def sizeOfLargestRegion(a: Vector[Vector[Node]], r: Int = 0, c: Int = 0): Int = {
    a.lift(r).flatMap(_.lift(c)) match {
      case Some(n) if !n.visited && n.value == 1 =>
        n.visited = true

        val neighbors = (-1 to 1).flatMap { i =>
          (-1 to 1).map { j =>
            sizeOfLargestRegion(a, r + i, c + j)
          }
        }

        1 + neighbors.sum
      case _ => 0
    }
  }
}
