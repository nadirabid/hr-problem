object Pairs {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val k = inputIter.next().split(" ")(1).toInt
    val s = inputIter.next().split(" ").map(_.toInt)

    println(numberOfPairsWithDifferenceOf(k, s.sorted))
    println(numberOfPairsWithDifferenceOf2(k, s))
  }

  def numberOfPairsWithDifferenceOf2(k: Int, s: Array[Int]): Int = {
    var numOfPairs = 0

    for (i <- s.indices)
      for (j <- i + 1 until s.length)
        if (math.abs(s(i) - s(j)) == k)
          numOfPairs += 1

    numOfPairs
  }

  def numberOfPairsWithDifferenceOf(k: Int, s: Array[Int]): Int = {
    s.indices.foldLeft(0) { (numOfPairs, i) =>
      var left = i
      var right = s.length - 1
      var j = i + (right - left) / 2
      var found = false

      if (s(left) - s(i) == k || s(right) - s(i) == k)
        found = true

      while (right - left > 1 && !found) {
        if (s(j) - s(i) == k) {
          found = true
        }
        else if(s(j) - s(i) > k) {
          right = j
          j = j - (right - left) / 2
        }
        else {
          left = j
          j = j + (right - left) / 2
        }
      }

      if (found) numOfPairs + 1 else numOfPairs
    }
  }
}
