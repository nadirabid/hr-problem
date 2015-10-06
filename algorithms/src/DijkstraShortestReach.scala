import scala.collection.mutable.ArrayBuffer

object DijkstraShortestReach {
  case class Edge(node: Int, distance: Int)
  case class Node(edges: List[Edge] = Nil, var visited: Boolean = false, var distance: Int = Int.MaxValue)

  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val t = inputIter.next().toInt

    var i = 0
    for (ln <- inputIter) {

      val n +: m +: _ = ln.split(" ").map(_.toInt).toIndexedSeq

      val nodesArr = inputIter.take(m)
        .map(_.split(" ").map(_.toInt))
        .foldLeft(Array.fill(n)(Node())) { case (nodes, Array(x, y, r)) =>
          val sX = x - 1
          val sY = y - 1

          nodes(sX) = Node(Edge(sY, r) :: nodes(sX).edges)
          nodes(sY) = Node(Edge(sX, r) :: nodes(sY).edges)

          nodes
        }


      val startNode = inputIter.next().toInt - 1
      if (i == 2) {
        dijkstraShortestReachFromStart(startNode, nodesArr)

        nodesArr.indices.foreach { i =>
          if (i != startNode) {
            if (nodesArr(i).distance == Int.MaxValue)
              print("-1 ")
            else
              print(s"${nodesArr(i).distance} ")
          }
        }

        println()
      }
      i += 1
    }
  }

  def dijkstraShortestReachFromStart(start: Int, nodes: Array[Node]): Unit = {
    nodes(start).distance = 0
    val q = ArrayBuffer(nodes.indices:_*)

    while (q.nonEmpty) {
      val i = q.remove(q.indices.minBy(i => nodes(q(i)).distance))
      val n = nodes(i)
      n.visited = true

      if (n.distance != Int.MaxValue) {
        n.edges.foreach { case Edge(neighbor, distance) =>
          if (!nodes(neighbor).visited) {
            nodes(neighbor).distance = math.min(nodes(neighbor).distance, n.distance + distance)
          }
        }
      }
    }
  }
}
