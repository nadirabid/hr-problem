object BotSavesPrincess2 {

  def main(args: Array[String]): Unit = {
    val input = io.Source.fromFile("ai/input/botsavesprincess2.txt").getLines()
    val m = input.next().toInt
    val botY +: botX +: _ = input.next().split(" ").map(_.toInt).toIndexedSeq

    var princessX = -1
    var princessY = -1

    for (ln <- input.take(m).takeWhile(_ => princessX < 0)) {
      princessY += 1
      princessX = ln.indexOf('p')
    }

    val x = princessX - botX
    val y = princessY - botY

    println(if (x > 0) "RIGHT" else if (x < 0) "LEFT" else if (y > 0) "DOWN" else if (y < 0) "UP")
  }

}
