package sbosley.euler.p1to50.p31to40.problem33

import sbosley.euler.math.Primes

object DigitCancellingFractions {

  def main(args: Array[String]): Unit = {
    val fractions = findDigitCancellingFractions
    val (productA, productB) = fractions.foldLeft((1, 1)) { case ((prodNum, prodDenom), (a, b)) =>
      (prodNum * a, prodDenom * b)
    }
    val (reducedA, reducedB) = reduceFraction(productA, productB)
    println(s"$reducedA / $reducedB")
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
      reduceFraction(a, b) == reduceFraction(fakeReducedA, fakeReducedB)
    }
  }

  def reduceFraction(num: Int, den: Int): (Int, Int) = {
    val divisors = (Primes.allDivisors(num) - 1).toList.sorted.reverse
    if (divisors.isEmpty) (num, den)
    else divisors.foldLeft((num, den)) { case ((a, b), div) =>
      if (a != 0 && b != 0 && a % div == 0 && b % div == 0) (a / div, b / div)
      else (a, b)
    }
  }

}
