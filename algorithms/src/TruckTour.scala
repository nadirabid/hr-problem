object TruckTour {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()
    val t = inputIter.next().toInt

    val pumps = inputIter.take(t).map(_.split(" ").map(_.toLong)).map {
      case Array(fuel, distance) => (fuel, distance)
    }.toIndexedSeq

    println(findTour(pumps))
    println(findTour2(pumps))
  }

  def findTour(pumps: Seq[(Long, Long)]): Int = {
    for (i <- pumps.indices) {
      var fuelRemaining = 0L
      for (
        j <- pumps.indices;
        (fuel, distToNextPump) <- pumps.lift((j + i) % pumps.length)
      ) {
        if (fuelRemaining >= 0) {
          fuelRemaining += (fuel - distToNextPump)
        }
      }

      if (fuelRemaining >= 0) return i
    }

    -1
  }

  def findTour2(pumps: Seq[(Long, Long)]): Int = {
    var j = 0
    var remainingFuel = 0L

    val gasRemainingAt = Array.fill(pumps.length)(0L)

    for ((fuel, distToNextPump) <- pumps) {
      gasRemainingAt(j) = remainingFuel
      remainingFuel += (fuel - distToNextPump)
      j += 1
    }

    gasRemainingAt.view.zipWithIndex.min._2
  }
}
