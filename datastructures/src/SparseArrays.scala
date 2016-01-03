object SparseArrays {
  import scala.collection.mutable.Map

  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("datastructures/input.txt").getLines()

    val n = inputIter.next().toInt
    val dictionary = inputIter.take(n).foldLeft(Map[String, Int]()) { (d, s) =>
      d(s) = if (d.contains(s)) d(s) + 1 else 1
      d
    }

    val q = inputIter.next().toInt

    inputIter.take(q).foreach { query =>
      println(dictionary.lift(query).getOrElse(0))
    }
  }
}
