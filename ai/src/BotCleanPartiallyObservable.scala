object BotCleanPartiallyObservable {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("ai/input.txt").getLines()
    val bY +: bX +: _ = inputIter.next().split(" ").map(_.toInt).toIndexedSeq

    var dY = -1
    var dX = -1

    for(ln <- inputIter.takeWhile(_ => dX < 0)) {
      dX = ln.indexOf('d')
      dY += 1
    }

    println(if (dX >= 0) nextMoveToSpot(bY, bX, dY, dX) else middleRingInnerHeuristicNextMove(bY, bX))
  }

  def middleRingInnerHeuristicNextMove(bY: Int, bX: Int): String = {
    val innerRing = IndexedSeq((1,1), (1,2), (1, 3), (2, 3), (3, 3), (3, 2), (3, 1), (2, 1))

    val (innerRingY, innerRingX) = innerRing.minBy { case (innerRingY:Int, innerRingX:Int) =>
      Math.abs(innerRingX - bX) + Math.abs(innerRingY - bY)
    }

    if (innerRingX - bX == 0 && innerRingY - bY == 0) {
      moveClockwiseOnInnerRing(bY, bX)
    }
    else {
      nextMoveToSpot(bY, bX, innerRingY, innerRingX)
    }
  }

  def moveClockwiseOnInnerRing(bY: Int, bX: Int): String = {
    if (bX < 3 && bY == 1) "RIGHT"
    else if (bX == 3 && bY < 3) "DOWN"
    else if (bX > 1 && bY == 3) "LEFT"
    else "UP"
  }

  def nextMoveToSpot(bY: Int, bX: Int, targetY: Int, targetX: Int): String = {
    if (targetX - bX > 0) "RIGHT"
    else if (targetX - bX < 0) "LEFT"
    else if (targetY - bY > 0) "DOWN"
    else if (targetY - bY < 0) "UP"
    else "CLEAN"
  }
}
