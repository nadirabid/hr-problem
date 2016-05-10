object TwoDimArrayDS {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("datastructures/input.txt").getLines()

    val mat = inputIter.map(_.split(" ").map(_.toInt).toVector).toVector

    println(mat.indices.foldLeft(Int.MinValue) { (rm, r) =>
      math.max(rm, mat(r).indices.foldLeft(Int.MinValue) { (cm, c) =>
        math.max(cm, hourGlassValueOfCell(mat, r, c))
      })
    })
  }

  def hourGlassValueOfCell(mat: Vector[Vector[Int]], m: Int, n: Int): Int = {
    if (m + 2 < mat.length && n + 2 < mat.head.length) {
      val top = (n to n + 2).foldLeft(0)((sum, i) => sum + mat(m)(i))
      val bottom = (n to n + 2).foldLeft(0)((sum, i) => sum + mat(m + 2)(i))

      return top + mat(m + 1)(n + 1) + bottom
    }

    Int.MinValue
  }
}
