object AlmostSorted {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()
    inputIter.next().trim.toInt

    singleSwapOrReverse(inputIter.next().split(" ").map(_.toInt)) match {
      case Some(("", -1, -1)) => println("yes")
      case Some((op, l, r)) => println("yes"); println(s"$op $l $r")
      case None => println("no")
    }
  }

  def singleSwapOrReverse(ints: Array[Int]): Option[(String, Int, Int)] = {
    var l = -1
    var r = -1

    var flips = 0

    for (i <- 0 until ints.length - 1; curr = ints(i); next = ints(i + 1)) {
      if (flips % 2 == 0 && curr > next) {
        if (l == -1) l = i
        else if (l > -1) r = i

        flips += 1
      }
      else if (flips % 2 == 1 && curr < next) {
        r = i
        flips += 1
      }
    }

    if (ints(ints.length - 2) > ints(ints.length - 1)) {
      if (flips % 2 == 0) flips += 1
      r = ints.length - 1
    }

    if (flips == 0) {
      Some(("", -1, -1))
    }
    else if (flips == 1 && ints.length == 2) {
      Some(("swap", 1, 2))
    }
    else if (flips == 2 && ints.length > 4 && l + 1 != r) {
      if (r + 1 == ints.length || ints(l) < ints(r + 1))
        Some(("reverse", l + 1, r + 1))
      else
        None
    }
    else if (flips <= 4) {
      if (r + 1 == ints.length || ints(l) < ints(r + 1))
        Some(("swap", l + 1, r + 1))
      else
        None
    }
    else {
      None
    }
  }

  def checkIsSorted(ints: Array[Int], i: Int = 1): Boolean = {
    if (i == ints.length)
      true
    else if (ints(i - 1) < ints(i))
      checkIsSorted(ints, i + 1)
    else
      false
  }
}
