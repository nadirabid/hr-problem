object CoinChanging {
  import scala.collection.mutable

  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val n +: m +: _ = inputIter.next().split(" ").map(_.toInt).toSeq
    val coins = inputIter.next().split(" ").map(_.toInt).toVector

    val ways = coins.foldLeft(mutable.Map[Int, Array[Long]]()) { (w, c) =>
      w(c) = Array.fill(n + 1)(-1L)
      w
    }

    println(numOfWaysToMakeChange(coins, n, ways))
  }

  def numOfWaysToMakeChange(coins: Vector[Int], n: Int, ways: mutable.Map[Int, Array[Long]], coinIndex: Int = 0): Long = {
    coins.lift(coinIndex) match {
      case Some(c) =>
        ways(c)(n) = (0 to n).foldLeft(0L) { (numOfWays, i) =>
          val remaining = n - c*i

          if (coinIndex + 1 < coins.length && ways(coins(coinIndex + 1)).lift(remaining).getOrElse(-1L) >= 0) {
            numOfWays + ways(coins(coinIndex + 1))(remaining)
          }
          else if (remaining > 0) {
            numOfWays + numOfWaysToMakeChange(coins, remaining, ways, coinIndex + 1)
          }
          else if (remaining == 0) {
            numOfWays + 1
          }
          else {
            numOfWays
          }
        }

        ways(c)(n)
      case None =>
        0
    }
  }
}
