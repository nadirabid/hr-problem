object TimeConversion {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()
    val time = inputIter.next()

    val period = time.substring(8, 10)

    var hours = time.substring(0, 2).toInt

    if (period == "AM")
      hours %= 12
    else
      hours = (hours % 12) + 12

    if (hours < 12)
      println(s"0$hours:${time.substring(3, 8)}")
    else
      println(s"$hours:${time.substring(3,8)}")
  }
}
