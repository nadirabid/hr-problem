object LeastCommonAncestor {
  import scala.collection.mutable

  case class Node(left: Option[Node], right: Option[Node], parent: Option[Node])

  def lowestCommonAncestor(a: Node, b: Node): Node = {
    val pathToRootFromA = getPathToRoot(a)
    val pathToRootFromB = getPathToRoot(b)

    val shortestPath = math.min(pathToRootFromA.length, pathToRootFromB.length)

    var prev: Node = null

    for (_ <- 0 until shortestPath) {
      val nodeFromPathA = pathToRootFromA.pop()
      val nodeFromPathB = pathToRootFromB.pop()

      if (nodeFromPathA == nodeFromPathB)
        prev = nodeFromPathA
    }

    prev
  }

  def getPathToRoot(n: Node, s: mutable.Stack[Node] = mutable.Stack()): mutable.Stack[Node] = {
    s.push(n)

    n.parent match {
      case Some(parent) =>
        getPathToRoot(parent, s)
      case None =>
        s
    }
  }
}
