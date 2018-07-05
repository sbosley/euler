package sbosley.euler.p1to50.p31to40.problem34

import sbosley.euler.util.math.MathHelpers

object DigitFactorials {

  def main(args: Array[String]): Unit = {
    val upperBound = 999999 // Because even adding another 9 can only add an additional 9! = 362880 to the digit sum
    val sum = (3 to upperBound).filter(n => isDigitFactorial(n.toLong)).sum
    println(sum)
  }

  private def isDigitFactorial(n: Long): Boolean = {
    n.toString.map(_.asDigit).map(d => MathHelpers.factorial(d.toLong)).sum == n
  }

}
