object Encryption {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()
    val s = inputIter.next()

    println(encrypt(s))
  }

  def encrypt(s: String): String = {
    val a = math.floor(math.sqrt(s.length)).toInt
    val b = math.ceil(math.sqrt(s.length)).toInt

    val rows = if (a * b < s.length) a + 1 else a
    val cols = b

    val g = Array.fill(cols, rows)("")

    for (r <- 0 until rows)
      for (c <- 0 until cols)
        if (r*cols + c < s.length)
          g(c)(r) = s(r*cols + c).toString

    g.foldLeft("") { (s, c) =>
      if (s.length > 0)
        s"$s ${c.mkString}"
      else
        c.mkString
    }
  }
}
