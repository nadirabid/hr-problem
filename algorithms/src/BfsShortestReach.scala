object BfsShortestReach {
  case class Node(neighbors: List[Int] = Nil, var distance: Int = -1)
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val t = inputIter.next().toInt

    for (ln <- inputIter) {
      val n +: m +: _ = ln.split(" ").map(_.toInt).toIndexedSeq
      val nodes = inputIter
        .take(m)
        .map(_.split(" ").map(_.toInt))
        .foldLeft(Array.fill(n)(Node())) {
          case (nodesArr, Array(a, b)) =>
            nodesArr(a - 1) = Node((b - 1) :: nodesArr(a - 1).neighbors)
            nodesArr(b - 1) = Node((a - 1) :: nodesArr(b - 1).neighbors)

            nodesArr
        }

      val startNode = inputIter.next().toInt
      updatesNodeWithShortestReachFromNode(startNode - 1, nodes)
    }
  }

  def updatesNodeWithShortestReachFromNode(start: Int, nodes: Array[Node]): Unit = {
    val startNode = nodes(start)
    val q = scala.collection.mutable.Queue[Node](startNode)

    startNode.distance = 0

    while (q.nonEmpty) {
      val n = q.dequeue()
      n.neighbors.view
        .map(nodes)
        .withFilter(_.distance < 0)
        .foreach { neighbor =>
          neighbor.distance = n.distance + 6
          q.enqueue(neighbor)
        }
    }

    nodes.indices.foreach { n =>
      if (n != start) print(s"${nodes(n).distance} ")
    }

    print("\n")
  }
}
