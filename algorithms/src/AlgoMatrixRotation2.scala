object AlgoMatrixRotation2 {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()
    val m :: n :: r :: _ = inputIter.next().split(" ").map(_.toInt).toList
    val mat = inputIter.take(m).map(_.split(" ").map(_.toInt)).toArray

    for (ring <- 0 until math.min(m/2, n/2)) {
      val rotations = r % ((m - ring*2 - 1)*2 + (n - ring*2 - 1)*2)

      for (_ <- 0 until rotations)
        rotateMatrix(mat, m, n, ring)
    }

    mat.foreach(r => println(r.mkString(" ")))
  }

  def rotateMatrix(mat: Array[Array[Int]], m: Int, n: Int, ring: Int): Unit = {
    val maxI = n - ring
    val maxJ = m - ring

    val carryOver = mat(ring)(ring)

    for (i <- (ring + 1) until maxI)
      mat(ring)(i - 1) = mat(ring)(i)

    for (j <- (ring + 1) until maxJ)
      mat(j - 1)(maxI - 1) = mat(j)(maxI - 1)

    for (i <- (maxI - 1) until ring by -1)
      mat(maxJ - 1)(i) = mat(maxJ - 1)(i - 1)

    for (j <- (maxJ - 1) until ring by -1)
      mat(j)(ring) = mat(j - 1)(ring)

    mat(ring + 1)(ring) = carryOver
  }
}
