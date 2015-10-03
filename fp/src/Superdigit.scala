object Superdigit {
  def main(args: Array[String]): Unit = {
    val n +: k +: _ = io.Source.fromFile("fp/input.txt").getLines().next().split(" ").toIndexedSeq

    val p = BigInt(n.foldLeft(0)((sd, digit) => sd + (digit - '0'))) * BigInt(k)

    println(superDigit(p))
  }

  def superDigit(p: BigInt): BigInt = {
    var p2 = p
    var sd = BigInt(0)

    while (p2 > 0) {
      sd += p2 mod 10
      p2 = p2 / 10
    }

    if (sd < 10) sd else superDigit(sd)
  }
}
