package sbosley.euler.p51to100.p51to60.problem58

import sbosley.euler.util.math.MathHelpers.Fraction
import sbosley.euler.util.math.Primes

import scala.annotation.tailrec

object SpiralPrimes {

  // 37 36 35 34 33 32 31
  // 38 17 16 15 14 13 30
  // 39 18  5  4  3 12 29
  // 40 19  6  1  2 11 28
  // 41 20  7  8  9 10 27
  // 42 21 22 23 24 25 26
  // 43 44 45 46 47 48 49
  //
  // In an n x n square:
  // Upper right = (n - 2) ^ 2 + (n - 1)
  // Upper left = upper right + (n - 1) = (n - 2) ^ 2 + 2 * (n - 1)
  // Lower left = Upper left + (n - 1) = (n - 2) ^ 2 + 3 * (n - 1)
  // Lower right = n^2

  private val primesSet = Primes.primesToMax(700000000).map(_.toLong).toSet

  def main(args: Array[String]): Unit = {
    println(findPrimeRatioRecursive(7, Fraction(8, 13)))
  }

  @tailrec
  private def findPrimeRatioRecursive(sideLength: Long, frac: Fraction): Long = {
    if (frac < Fraction(1, 10)) sideLength
    else {
      val newSideLength = sideLength + 2
      val lastSideSquare = sideLength * sideLength
      val newValues = Set(
        lastSideSquare + (newSideLength - 1),
        lastSideSquare + 2 * (newSideLength - 1),
        lastSideSquare + 3 * (newSideLength - 1),
        newSideLength * newSideLength
      )
      val primeCount = newValues.count(primesSet)
      findPrimeRatioRecursive(newSideLength, Fraction(frac.num + primeCount, frac.den + 4))
    }
  }
}
