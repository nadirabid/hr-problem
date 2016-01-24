object CastleInTheGrid {
  import scala.collection.mutable

  case class Node(value: String, var step: Int = -1, var visited: Boolean = false)

  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("datastructures/input.txt").getLines()

    val n = inputIter.next().toInt
    val g = inputIter.take(n).map(_.split("").slice(0, n).map(Node(_)).toVector).toVector
    val a :: b :: c :: d :: _ = inputIter.next().split(" ").map(_.toInt).toList

    println(numOfStepsToMoveCastle(g, a, b, c, d))
  }

  def numOfStepsToMoveCastle(g: Vector[Vector[Node]], a: Int, b: Int, c: Int, d: Int): Int = {
    val q = mutable.Queue[(Int, Int)]()

    val moves = Vector((1, 0), (0, 1), (-1, 0), (0, -1))

    g(a)(b).step = 0
    q.enqueue((a, b))

    while (q.nonEmpty) {
      val (y, x) = q.dequeue()
      g(y)(x).visited = true

      if (y == c && x == d) {
        return g(y)(x).step
      }

      val nextStep = g(y)(x).step + 1
      moves.foreach { case (moveY, moveX) =>
        var (nextY, nextX) = (y + moveY, x + moveX)

        while (nextY >= 0 && nextX >= 0
            && nextY < g.length && nextX < g.length
            && g(nextY)(nextX).value == ".") {

          if (!g(nextY)(nextX).visited) {
            g(nextY)(nextX).step = nextStep
            q.enqueue((nextY, nextX))
            g(nextY)(nextX).visited = true
          }

          nextY += moveY
          nextX += moveX
        }
      }
    }

    -1
  }
}
