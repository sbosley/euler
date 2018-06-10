package sbosley.euler.p1to50.p21to30.problem30

object DigitFifthPowers {

  def main(args: Array[String]): Unit = {
    val upperBound = 999999 // Because even adding another 9 can only add an additional 9^5 = 59049 to the digit sum
    val sum = (2 to upperBound).filter(isDigitFifthPower).sum
    println(sum)
  }

  private def isDigitFifthPower(n: Int): Boolean = {
    n.toString.map(_.asDigit).map(d => Math.pow(d, 5).toInt).sum == n
  }

}
