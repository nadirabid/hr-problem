import scala.collection.mutable

object LRU {
  class LeastRecentlyUsedCache[K, V](size: Int) {
    case class Node(var left: Option[Node], var right: Option[Node], key: K, item: V)

    var count = 0
    var nodes: Option[Node] = None
    var nodesMap = mutable.HashMap[K, Node]()

    def add(key: K, value: V): Unit = {
      if (count == size) {
        remove(nodes.head.key)
      }

      remove(key)

      nodes match {
        case head @ Some(node) =>
          nodesMap(key) = Node(None, head, key, value)
          nodes = nodesMap.lift(key)
          node.left = nodes
        case None =>
          nodesMap(key) = Node(None, None, key, value)
          nodes = nodesMap.lift(key)
      }

      count += 1
    }

    def remove(key: K): Option[V] = {
      val cacheEntry = nodesMap.remove(key)

      cacheEntry match {
        case Some(Node(Some(left), Some(right), _)) =>
          left.right = Some(right)
          right.left = Some(left)
        case Some(Node(Some(left), None, _)) =>
          left.right = None
        case Some(Node(None, Some(right), _)) =>
          nodes = Some(right)
        case Some(Node(None, None, _)) =>
          nodes = None
      }

      count -= 1

      cacheEntry.map(_.item)
    }
  }
}
