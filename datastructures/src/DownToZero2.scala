object DownToZero2 {
  import scala.collection.mutable

  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("datastructures/input.txt").getLines()
    val q = inputIter.next().toInt

    inputIter.take(q).map(_.toInt).foreach(n => println(downToZero(n)))
  }

  case class Node(n: Int, degree: Int)

  def downToZero(n: Int): Int = {
    val q = mutable.Queue[Node](Node(n, 0))
    val seen = mutable.Set[Int]()

    while (q.nonEmpty) {
      val curr = q.dequeue()

      if (curr.n == 0)
        return curr.degree

      q.enqueue(Node(curr.n - 1, curr.degree + 1))

      findFactors(curr.n)
        .diff(seen)
        .foreach { f =>
          q.enqueue(Node(f, curr.degree + 1))
          seen.add(f)
        }
    }

    -1
  }

  def findFactors(n: Int): mutable.Set[Int] = {
    (2 to math.sqrt(n).toInt).foldLeft(mutable.Set[Int]()) { (factors, f) =>
      if (n % f == 0)
        factors.add(math.max(f, n/f))

      factors
    }
  }
}
