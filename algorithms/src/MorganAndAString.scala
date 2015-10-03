object MorganAndAString {
  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("algorithms/input.txt").getLines()

    val t = inputIter.next().toInt

    for (str1 <- inputIter.take(t).map(_.toList)) {
      val str2 = inputIter.next().toList
      val r2 = lexicallyMinString2(str1, str2)
      println(r2)
    }
  }

  def lexicallyMinString2(s1: List[Char], s2: List[Char], r: List[Char] = Nil): String = {
    if (s1.isEmpty && s2.isEmpty) {
      r.reverse.mkString
    }
    else if (s1.isEmpty) {
      (r.reverse ::: s2).mkString
    }
    else if (s2.isEmpty) {
      (r.reverse ::: s1).mkString
    }
    else if (s1.head < s2.head) {
      lexicallyMinString2(s1.tail, s2, s1.head :: r)
    }
    else if (s2.head < s1.head) {
      lexicallyMinString2(s1, s2.tail, s2.head :: r)
    }
    else {
      val (c1, c2, newR) = helper(s1, s2, r)
      lexicallyMinString2(c1, c2, newR)
    }
  }

  def helper(s1: List[Char], s2: List[Char], r: List[Char]): (List[Char], List[Char], List[Char]) = {
    var prevChar = s1.head
    var c1 = s1
    var c2 = s2
    var j = 0

    while (c1.nonEmpty && c2.nonEmpty && c1.head == c2.head) {
      j += 1
      prevChar = c1.head

      c1 = c1.tail
      c2 = c2.tail
    }

    val pickFirstString = if (c1.isEmpty && c2.isEmpty) {
      true
    }
    else if (c2.isEmpty) {
      true
    }
    else if (c1.nonEmpty && c2.nonEmpty && c1.head < c2.head) {
      true
    }
    else {
      false
    }

    var ret = r
    var m, n = 0
    c1 = s1
    c2 = s2

    while (m < j && n < j) {
      if (c1.head < c2.head || (c1.head == c2.head && pickFirstString)) {
        ret = c1.head :: ret
        c1 = c1.tail
        m += 1
      }
      else {
        ret = c2.head :: ret
        c2 = c2.tail
        n += 1
      }
    }

    (c1, c2, ret)
  }
}



