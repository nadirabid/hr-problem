object SierpinskiTriangles {
  def main(args: Array[String]): Unit = {
    val n = 4
    drawTriangles(n)
  }

  def drawTriangles(n:Int): Unit = {
    val mat = Vector.fill(32, 63)('-').zipWithIndex.map{ case (row, i) =>
      val midPoint = 63/2
      row.zipWithIndex.map { case (e, j) =>
        if (midPoint - i <= j && j <= midPoint + i) '1' else '-'
      }
    }
  }

}
