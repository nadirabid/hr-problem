object ValidBST {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("fp/input.txt").getLines()

    val t = inputIter.next().toInt

    for(n <- inputIter) {
      val traversal = inputIter.next().split(" ").map(_.toInt).toIndexedSeq
      println(isValidBSTPreorderTraversal(traversal.head, traversal.drop(1)))
    }
  }

  def isValidBSTPreorderTraversal(r: Int, t: IndexedSeq[Int]): Boolean = {
    if (t.nonEmpty) {
      val left = t.takeWhile(_ < r)
      val right = t.dropWhile(_ < r)

      if (!right.exists(_ < r)) {
        (if (left.nonEmpty) isValidBSTPreorderTraversal(left.head, left.drop(1)) else true) &&
          (if (right.nonEmpty) isValidBSTPreorderTraversal(right.head, right.drop(1)) else true)
      }
      else {
        false
      }
    }
    else {
      true
    }
  }
}
