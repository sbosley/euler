package sbosley.euler.p1to50.p31to40.problem33

import sbosley.euler.util.math.MathHelpers.Fraction

object DigitCancellingFractions {

  def main(args: Array[String]): Unit = {
    val fractions = findDigitCancellingFractions
    val (productA, productB) = fractions.foldLeft((1, 1)) { case ((prodNum, prodDenom), (a, b)) =>
      (prodNum * a, prodDenom * b)
    }
    println(s"${Fraction(productA, productB).reduce}")
    // 16 / 64
    // 26 / 65
    // 19 / 95
    // 49 / 98
  }

  def findDigitCancellingFractions: Seq[(Int, Int)] = {
    for {
      b <- 11 to 99
      a <- 10 until b
      if isDigitCancellingFraction(a, b)
    } yield {
      (a, b)
    }
  }

  def isDigitCancellingFraction(a: Int, b: Int): Boolean = {
    val aStr = a.toString
    val bStr = b.toString

    val aChars = aStr.toSet
    val bChars = bStr.toSet
    val charIntersection = aChars.intersect(bChars)

    if (charIntersection.isEmpty || charIntersection.size == 2 || charIntersection('0')) false
    else {
      val intersectDigit = charIntersection.head
      val fakeReducedA = (aChars - intersectDigit).headOption.getOrElse(intersectDigit).asDigit
      val fakeReducedB = (bChars - intersectDigit).headOption.getOrElse(intersectDigit).asDigit
      Fraction(a, b).reduce == Fraction(fakeReducedA, fakeReducedB).reduce
    }
  }

}
