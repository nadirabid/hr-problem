object TheTimeInWords {

  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()
    val h = inputIter.next().trim.toInt
    val m = inputIter.next().trim.toInt

    println(timeInWords(h, m))
  }

  def timeInWords(h: Int, m: Int): String = {
    if (m == 0) {
      s"${numberToName(h)} o' clock"
    }
    else if (m == 1) {
      s"one minute past ${numberToName(h)}"
    }
    else if (m == 15) {
      s"quarter past ${numberToName(h)}"
    }
    else if (m < 30) {
      s"${numberToName(m)} minutes past ${numberToName(h)}"
    }
    else if (m == 30) {
      s"half past ${numberToName(h)}"
    }
    else if (m == 45) {
      s"quarter to ${numberToName(if (h == 12) 1 else h + 1)}"
    }
    else if (m == 59) {
      s"one minute to ${numberToName(if (h == 12) 1 else h + 1)}"
    }
    else {
      s"${numberToName(60 - m)} minutes to ${numberToName(if (h == 12) 1 else h + 1)}"
    }
  }

  def numberToName(num: Int): String = {
    val numNames = Vector(
      "", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
      "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"
    )

    val tensNumNames = Vector(
      "", "", "twenty", "thirty", "fourty", "fifty", "sixty"
    )

    if (num < 20) {
      numNames(num)
    }
    else if (num % 10 == 0) {
      tensNumNames(num / 10)
    }
    else {
      s"${tensNumNames(num / 10)} ${numNames(num % 10)}"
    }
  }
}
