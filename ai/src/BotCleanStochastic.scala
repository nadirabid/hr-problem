object BotCleanStochastic {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("ai/input.txt").getLines()
    val bY +: bX +: _ = inputIter.next().split(" ").map(_.toInt).toIndexedSeq

    var dY = -1
    var dX = -1

    for(ln <- inputIter.takeWhile(_ => dX < 0)) {
      dX = ln.indexOf('d')
      dY += 1
    }

    if (dX - bX > 0)
      println("RIGHT")
    else if (dX - bX < 0)
      println("LEFT")
    else if (dY - bY > 0)
      println("DOWN")
    else if (dY - bY < 0)
      println("UP")
    else
      println("CLEAN")
  }
}
