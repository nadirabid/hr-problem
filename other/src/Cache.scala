import scala.collection.mutable

class Cache(size: Int) {
  private var count = 0
  private val nodesMap = mutable.HashMap[Int, Node]()
  private var head: Option[Node] = None
  private var tail: Option[Node] = None

  case class Node(key: Int, value: Int, var prev: Option[Node], var next: Option[Node])

  def get(key: Int): Option[Int] = {
    nodesMap.lift(key).map(_.value)
  }

  def set(key: Int, value: Int) = {
    if (size == count && !nodesMap.contains(key)) {
      remove(tail.get.key)
    }

    remove(key)

    val entry = Node(key, value, None, head)
    head = Some(entry)

    if (tail == None) {
      tail = Some(entry)
    }

    nodesMap.put(key, entry)
    count += 1
  }

  def remove(key: Int): Option[Int] = {
    val entry = nodesMap.remove(key)

    entry match {
      case Some(Node(_, _, Some(prev), Some(next))) => {
        prev.next = Some(next)
        next.prev = Some(prev)
      }
      case Some(Node(_, _, None, Some(next))) => {
        head = Some(next)
        next.prev = None
      }
      case Some(Node(_, _, Some(prev), None)) => {
        tail = Some(prev)
        prev.next = None
      }
      case Some(Node(_, _, None, None)) => {
        head = None
        tail = None
      }
    }

    entry match {
      case Some(n) =>
        count -= 1
        Some(n.value)
      case None =>
        None
    }
  }

}
