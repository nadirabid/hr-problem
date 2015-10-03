object StringSimilarity {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()
    val t = inputIter.next().toInt

    for (s <- inputIter.take(t)) {
      println(z(s.trim))
    }
  }

  def z(s: String): Long = {
    val n = s.length
    val z = Array.fill[Long](n)(0)
    var L, R = 0

    z(0) = n

    for (i <- 1 until n) {
      if (i > R) {
        L = i
        R = i

        while (R < n && s(R-L) == s(R))
          R += 1

        z(i) = R-L
        R -= 1
      } else {
        val k = i-L

        if (z(k) < R-i+1) {
          z(i) = z(k)
        } else {
          L = i

          while (R < n && s(R-L) == s(R))
            R += 1

          z(i) = R-L
          R -= 1
        }
      }
    }

    z.sum
  }

  def stringSimilarity(s: String): Long = {
    (1 until s.length).foldLeft(s.length.toLong)(_ + matchSubstringToStringPrefix(s, _))
  }

  def matchSubstringToStringPrefix(s: String, offset: Int, i: Int = 0): Long = {
    if (i + offset < s.length && s(i) == s(offset + i))
      matchSubstringToStringPrefix(s, offset, i + 1)
    else
      i
  }
}
