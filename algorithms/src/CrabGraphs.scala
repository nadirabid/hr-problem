object CrabGraphs {
  case class Node(neighbors: Vector[Int] = Vector(), var visited: Boolean = false, var isCrab: Boolean = false)

  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val c = inputIter.next().trim.toInt

    for (ln <- inputIter) {
      val n +: t +: m +: _ = ln.split(" ").map(_.toInt).toSeq

      val nodes = inputIter
        .take(m)
        .map(_.split(" ").map(_.toInt))
        .foldLeft(Array.fill(n)(Node())) { case (nodes, Array(a, b)) =>
          nodes(a - 1) = Node((b - 1) +: nodes(a - 1).neighbors)
          nodes(b - 1) = Node((a - 1) +: nodes(b - 1).neighbors)

          nodes
        }

      println(findCrabs(nodes, t))
    }
  }

  def findCrabs(nodes: Array[Node], t: Int): Int = {


    0
  }

  def findCrabs(nodes: Array[Node], n: Int, t: Int): Int = {
    nodes(n).visited = true

    nodes(n).neighbors.foldLeft(0) { (count, neighbor) =>
      if (nodes(neighbor).visited) {

      }

      count
    }

    0
  }
}
