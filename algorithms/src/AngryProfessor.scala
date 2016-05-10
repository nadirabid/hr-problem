object AngryProfessor {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val t = inputIter.next().toInt

    for (k <- inputIter.take(t).map(_.split(" ").last.toInt)) {
      val arrivalTimes = inputIter.next().split(" ").map(_.toInt).toVector
      if (arrivalTimes.count(_ <= 0) >= k)
        println("NO")
      else
        println("YES")
    }
  }
}
