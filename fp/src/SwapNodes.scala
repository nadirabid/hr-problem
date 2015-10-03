object SwapNodes {
  case class Node(var left: Int, var right: Int)
  case class BTNode(left: Option[BTNode], right: Option[BTNode])

  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("fp/input.txt").getLines()

    val n = inputIter.next().toInt

    val nodesMap = inputIter
      .take(n)
      .map(_.split(" ").map(_.toInt))
      .zipWithIndex
      .map { case (Array(a, b), i) => (i + 1) -> Node(a, b) }
      .toMap

    val t = inputIter.next().toInt
    for (k <- inputIter.take(t).map(_.toInt)) {
      printInOrderTraversal(nodesMap, k, 1, 1)
    }
  }

  def createBinaryTree(nodesMap: Map[Int, Node], i: Int): Option[BTNode] = {
    nodesMap.lift(i).map { case Node(left, right) =>
      BTNode(createBinaryTree(nodesMap, left), createBinaryTree(nodesMap, right))
    }
  }

  def printInOrderTraversal(nodes: Map[Int, Node], k: Int, i: Int, h: Int): Unit = {
    nodes.lift(i).foreach { node =>
      if (h % k == 0) {
        val temp = node.left
        node.left = node.right
        node.right = temp
      }

      printInOrderTraversal(nodes: Map[Int, Node], k, node.left, h+1)
      print(s"$i ")
      printInOrderTraversal(nodes: Map[Int, Node], k, node.right, h+1)
    }

    if (i == 1) print("\n")
  }
}
