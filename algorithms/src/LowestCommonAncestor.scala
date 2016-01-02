object LowestCommonAncestor {
  case class Node(parent: Node, left: Option[Node], right: Option[Node])

  def findCommonAncestor(root: Node, a: Node, b: Node): Option[Node] = {
    val left = root.left.flatMap(findCommonAncestor(_, a, b))
    val right = root.right.flatMap(findCommonAncestor(_, a, b))

    if (left.isDefined)
      left
    else if (right.isDefined)
      right
    else
      None
  }
}
