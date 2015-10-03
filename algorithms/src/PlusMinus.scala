/**
 * Created by nadirmuzaffar on 9/17/15.
 */
object PlusMinus {
  def main(args: Array[String]) {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()
    val n = inputIter.next().toDouble
    val groups = inputIter.next().split(" ").map(_.toDouble).groupBy(math.signum)

    groups.lift(1) match {
      case Some(pos) => println(s"${math.rint(10000*pos.length/n)/10000}")
      case None => println("0.0000")
    }

    groups.lift(-1) match {
      case Some(neg) => println(s"${math.rint(10000*neg.length/n)/10000}")
      case None => println("0.0000")
    }

    groups.lift(0) match {
      case Some(zero) => println(s"${math.rint(10000*zero.length/n)/10000}")
      case None => println("0.0000")
    }
  }
}
