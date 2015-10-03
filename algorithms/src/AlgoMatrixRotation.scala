object AlgoMatrixRotation {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()
    val m +: n +: r +: _ = inputIter.next().split(" ").map(_.toInt).toIndexedSeq
    val mat = inputIter.take(m).map(_.split(" ").map(_.toInt)).toArray

    for (ringNumber <- 0 until Math.min(m/2, n/2)) {
      val rotations = r % ((m - ringNumber*2 - 1)*2 + (n - ringNumber*2 - 1)*2)

      for (_ <- 0 until rotations)
        rotateMatrixRing(mat, m, n, ringNumber)
    }

    mat.foreach(row => println(row.mkString(" ")))
  }

  def rotateMatrixRing(mat: Array[Array[Int]], m:Int, n:Int, ringNumber:Int): Unit = {
    val iMin = ringNumber
    val iMax = n - ringNumber - 1

    val jMin = ringNumber
    val jMax = m - ringNumber - 1

    val first = mat(jMin)(iMin)

    for (i <- iMin + 1 to iMax)
      mat(jMin)(i - 1) = mat(jMin)(i)

    for (j <- jMin + 1 to jMax)
      mat(j - 1)(iMax) = mat(j)(iMax)

    for (i <- iMax - 1 to iMin by -1)
      mat(jMax)(i + 1) = mat(jMax)(i)

    for (j <- jMax - 1 to jMin + 1 by -1)
      mat(j + 1)(iMin) = mat(j)(iMin)

    mat(jMin + 1)(iMin) = first
  }
}
