object MissingNumbers {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    inputIter.next().toInt
    val a = inputIter.next().split(" ").map(_.toInt)

    inputIter.next().toInt
    val b = inputIter.next().split(" ").map(_.toInt)

    findMissingLetters(a, b).foreach(e => print(s"$e "))
  }

  def findMissingLetters(a: Array[Int], b: Array[Int]): scala.collection.mutable.SortedSet[Int] = {
    val numbersInA = a.foldLeft(scala.collection.mutable.Map[Int, Int]()) { (numbersInA, num) =>
      numbersInA(num) = numbersInA.getOrElse(num, 0) + 1
      numbersInA
    }

    val numbersInB = b.foldLeft(scala.collection.mutable.Map[Int, Int]()) { (numbersInB, num) =>
      numbersInB(num) = numbersInB.getOrElse(num, 0) + 1
      numbersInB
    }

    numbersInB.keys.foldLeft(scala.collection.mutable.SortedSet[Int]()) { (missingNumbers, num) =>
      if (numbersInB.getOrElse(num, 0) - numbersInA.getOrElse(num, 0) != 0) {
        missingNumbers.add(num)
      }

      missingNumbers
    }
  }
}
