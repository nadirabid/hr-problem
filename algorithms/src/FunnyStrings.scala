object FunnyStrings {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val t = inputIter.next().toInt

    inputIter.take(t).foreach { s =>
      println(if (isFunny(s)) "Funny" else "Not Funny")
    }
  }

  def isFunny(s:String): Boolean = {
    for (i <- 0 until s.length) {
      if (i != s.length - 1 && math.abs(s(i + 1) - s(i)) != math.abs(s(s.length - 1 - (i + 1)) - s(s.length - 1 - i)))
        return false
    }

    true
  }
}
