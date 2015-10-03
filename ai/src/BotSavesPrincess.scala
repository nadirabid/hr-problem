object BotSavesPrincess {
  def main(args: Array[String]): Unit = {
    val consoleStream = io.Source.stdin.getLines()
    val m = consoleStream.next().toInt
    val grid = consoleStream.take(m).toIndexedSeq

    val positionOfPrincess =
      if (grid(0)(0) == 'p') {
        (0, 0)
      }
      else if (grid(0)(m - 1) == 'p') {
        (0, m - 1)
      }
      else if (grid(m - 1)(0) == 'p') {
        (m - 1, 0)
      }
      else {
        (m - 1, m - 1)
      }

    val midPoint = (m - 1) / 2

    val xSteps = positionOfPrincess._2 - midPoint
    for (i <- 0 until Math.abs(xSteps)) {
      println(if (xSteps > 0) "RIGHT" else "LEFT")
    }

    val ySteps = positionOfPrincess._1 - midPoint
    for (i <- 0 until Math.abs(ySteps)) {
      println(if (ySteps > 0) "DOWN" else "UP")
    }
  }
}
