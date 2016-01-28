object JourneyToTheMoon {
  case class Node(neighbors: List[Int] = Nil, var visited: Boolean = false)

  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()
    val Array(n, i) = inputIter.next().split(" ").map(_.toInt)

    val nodes = inputIter
      .take(i)
      .map(_.split(" ").map(_.toInt))
      .foldLeft(Array.fill(n)(Node())) { case (nodes, Array(x, y)) =>
        nodes(x) = Node(y :: nodes(x).neighbors)
        nodes(y) = Node(x :: nodes(y).neighbors)
        nodes
      }

    val astronautsPerCountry = numOfAstronautsPerCountry(nodes)
    val sums = Array.fill(astronautsPerCountry.length)(0)

    for (i <- astronautsPerCountry.length - 2 to 0 by -1) {
      sums(i) = sums(i + 1) + astronautsPerCountry(i + 1)
    }

    val numOfPairs = (0 until astronautsPerCountry.length - 1).foldLeft(0L) { (numOfPairs, i) =>
      numOfPairs + sums(i).toLong*astronautsPerCountry(i).toLong
    }

    println(numOfPairs)
  }

  def numOfAstronautsPerCountry(nodes: Array[Node]): Vector[Int] = {
    nodes.indices.foldLeft(Vector[Int]()) { (numOfAstronautsPerCountry, s) =>
      if (!nodes(s).visited) {
        sizeOfTree(nodes, s) +: numOfAstronautsPerCountry
      }
      else {
        numOfAstronautsPerCountry
      }
    }
  }

  def sizeOfTree(nodes: Array[Node], s: Int): Int = {
    nodes(s).visited = true

    nodes(s).neighbors.foldLeft(1) { (size, neighbor) =>
      if (!nodes(neighbor).visited)
        size + sizeOfTree(nodes, neighbor)
      else
        size
    }
  }
}
