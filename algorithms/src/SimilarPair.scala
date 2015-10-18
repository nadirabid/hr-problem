object SimilarPair {
  import scala.collection.mutable.ArrayBuffer
  case class Node(children: ArrayBuffer[Int] = ArrayBuffer(), var parent: Int = -1, var visited: Boolean = false)
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val n +: t +: _ = inputIter.next().split(" ").map(_.toInt).toSeq

    val nodes = inputIter.take(n - 1)
      .map(_.split(" ").map(_.toInt))
      .foldLeft(Array.fill(n)(Node())) { case (a, Array(parent, child)) =>
        a(parent - 1).children.append(child - 1)
        a(child - 1).parent = parent - 1
        a
      }

    println(numberOfSimilarPairs(nodes, t))
  }

  def numberOfSimilarPairs(nodes: Array[Node], t: Int): Long = {
    var count = 0

    for (n <- nodes.indices.filter(nodes(_).parent == -1)) {
      val stack = scala.collection.mutable.Stack[Int](n)
      val set = scala.collection.mutable.Set[Int]()
      val parentsWithChildrenStillInStack = Array.fill(nodes.length)(0)

      while (stack.nonEmpty) {


      }

      while (stack.nonEmpty) {
        val i = stack.pop()

        nodes(i).visited = true
        stack.pushAll(nodes(i).children.filter(!nodes(_).visited))

        if (nodes(i).children.nonEmpty) {
          val childrenCount = nodes(i).children.length
          set.foreach(parentsWithChildrenStillInStack(_) += childrenCount)
          parentsWithChildrenStillInStack(i) = nodes(i).children.length
          set.add(i)
        }

        count += set.foldLeft(0) { (c, j) =>
          if (i == j || math.abs(j - i) > t) {
            c
          }
          else {
            c + 1
          }
        }

        if (nodes(i).parent > -1) {
          set.foreach(parentsWithChildrenStillInStack(_) -= 1)

          if (parentsWithChildrenStillInStack(nodes(i).parent) == 0)
            set.remove(nodes(i).parent)
        }
      }
    }

    count
  }

}
