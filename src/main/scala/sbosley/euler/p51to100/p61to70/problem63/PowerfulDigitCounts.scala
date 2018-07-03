package sbosley.euler.p51to100.p61to70.problem63

object PowerfulDigitCounts {

  // This works because x >= 10 ^ y has >= y + 1 digits AND
  // once x^y falls below y digits, it will never regain ground
  def main(args: Array[String]): Unit = {
    val result = (1 to 9).map { x =>
      Stream.from(1).find(y => BigInt(x).pow(y).toString.length != y)
        .map(_ - 1)
        .getOrElse(0)
    }.sum
    println(result)
  }

}
