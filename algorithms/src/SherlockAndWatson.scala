object SherlockAndWatson {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val n +: k +: q +: _ = inputIter.next().split(" ").map(_.toInt).toSeq

    val a = inputIter.next().split(" ").map(_.toInt).toVector
    val rotatedA = a.drop(n - (k % n)) ++ a.take(n - (k % n))

    inputIter.take(q).map(_.toInt).foreach(idx => println(rotatedA(idx)))
  }
}
