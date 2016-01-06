object PrimsMSTSpecialSubtree {
  import scala.collection.mutable

  case class Edge(to: Int = -1, weight: Int = Int.MaxValue)
  case class Node(neighbors: List[Edge] = Nil, var visited: Boolean = false)

  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val n :: m :: _ = inputIter.next().split(" ").map(_.toInt).toList

    val nodes = inputIter
        .take(m)
        .map(_.split(" ").map(_.toInt))
        .foldLeft(Array.fill(n)(Node())) {
          case (nodesArr, Array(x, y, r)) =>
            nodesArr(x - 1) = Node(Edge(y - 1, r) :: nodesArr(x - 1).neighbors)
            nodesArr(y - 1) = Node(Edge(x - 1, r) :: nodesArr(y - 1).neighbors)

            nodesArr
        }

    val s = inputIter.next().toInt - 1

    println(findWeightOfSpecialMinimumSpanningTree(s, nodes))
  }

  def findWeightOfSpecialMinimumSpanningTree(s: Int, nodes: Array[Node]): Int = {
    val f = mutable.Set[Int](s)
    val q = nodes.indices.foldLeft(mutable.Set[Int]()) { (q, i) =>
      if (s != i) q.add(i)
      q
    }

    var mstWeight = 0

    while (q.nonEmpty) {
      val minEdgeToAdd = f.foldLeft(Edge()) { (currMinEdge, n) =>
        val edgesNotInF = nodes(n).neighbors.filterNot(e => f.contains(e.to))
        val minEdge = if (edgesNotInF.isEmpty) Edge() else edgesNotInF.minBy(_.weight)

        if (minEdge.weight < currMinEdge.weight)
          minEdge
        else
          currMinEdge
      }

      f.add(minEdgeToAdd.to)
      q.remove(minEdgeToAdd.to)

      mstWeight += minEdgeToAdd.weight
    }

    mstWeight
  }
}
