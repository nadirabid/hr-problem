object EvenTree {
  case class Node(children: Vector[Int] = Vector(), var size: Int = 0, var visited: Boolean = false)

  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val n +: m +: _ = inputIter.next().split(" ").map(_.toInt).toSeq

    val nodes = inputIter.map(_.split(" ").map(_.toInt)).foldLeft(Array.fill(n)(Node())) { case (a, Array(i, j)) =>
      a(i - 1) = Node(a(i - 1).children :+ (j - 1))
      a(j - 1) = Node(a(j - 1).children :+ (i - 1))

      a
    }

    println(nodes.indices.foldLeft(0) { (c, i) =>
      if (!nodes(i).visited) {
        c + makeEvenTrees(nodes, i)._2
      }
      else {
        c
      }
    })
  }

  def makeEvenTrees(a: Array[Node], n: Int = 0): (Int, Int) = {
    a(n).visited = true

    val (size, count) = a(n).children.foldLeft((1, 0)) { case ((size, count), i) =>
      if (a(i).visited) {
        (size, count)
      }
      else {
        val (subTreeSize, subTreeCount) = makeEvenTrees(a, i)

        if (subTreeSize % 2 == 0) {
          (size + subTreeSize, count + subTreeCount + 1)
        }
        else {
          (size + subTreeSize, count + subTreeCount)
        }
      }
    }

    a(n).size = size

    (size, count)
  }
}
