object UtopianTree {
  def main(args: Array[String]): Unit = {
    val inputItr = io.Source.fromFile("algorithms/input.txt").getLines()
    val t = inputItr.next().toInt

    inputItr.take(t).map(_.toInt).foreach { growthCycles =>
      val totalHeight = (0 until growthCycles).foldLeft(1) { (height, growthCycle) =>
        if (growthCycle % 2 == 0) height * 2 else height + 1
      }

      println(totalHeight)
    }
  }
}
