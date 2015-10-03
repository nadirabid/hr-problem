object CutTheTree {
  case class Node(value: Int, var visited: Boolean = false)
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val n = inputIter.next().toInt
    val vertices = inputIter.next().split(" ").map(v => Node(v.toInt))
    val edges = inputIter.take(n - 1).map(_.split(" ").map(_.toInt - 1)).toArray

    val edgesMap = edges.foldLeft(scala.collection.mutable.Map[Int, List[Array[Int]]]()) {
      case (edgeMap, edge @ Array(a, b)) =>
        edgeMap(a) = edge :: edgeMap.getOrElse(a, Nil)
        edgeMap(b) = edge :: edgeMap.getOrElse(b, Nil)

        edgeMap
    }


    println(findMinimalDiffTreeCut(0, vertices.view.map(_.value).sum, vertices, edgesMap)._1)
  }

  def findMinimalDiffTreeCut(i: Int, totalTreeValue: Int, v: Array[Node], e: scala.collection.mutable.Map[Int, List[Array[Int]]]): (Int, Int) = {
    v(i) match {
      case n if !n.visited =>
        n.visited = true

        val (minDiff, total) = e.getOrElse(i, Nil).foldLeft(Int.MaxValue, v(i).value) { case ((runningMinDiff, runningTotal), edge) =>
          val j = if (edge.head == i) edge.last else edge.head
          val (childNodeMinDiff, childNodeTotal) = findMinimalDiffTreeCut(j, totalTreeValue, v, e)
          (math.min(runningMinDiff, childNodeMinDiff), runningTotal + childNodeTotal)
        }

        (math.min(minDiff, math.abs((totalTreeValue - total) - total)), total)
      case _ => (Int.MaxValue, 0)
    }
  }
}
