package sbosley.euler.util.math

import sbosley.euler.util.math.MathHelpers.gcd

case class Fraction(num: BigInt, den: BigInt) {

  override def toString: String = s"$num / $den"

  import Numeric.Implicits._
  def +(n: Int): Fraction = this + Fraction.intToFraction(n)
  def *(n: Int): Fraction = this * Fraction.intToFraction(n)
  def invert: Fraction = Fraction(den, num)
  def reduce: Fraction = {
    val divisor = gcd(num.abs, den)
    Fraction(num.signum * num / divisor, den / divisor)
  }
}

object Fraction {

    implicit def intToFraction(n: Int): Fraction = Fraction(n, 1)
    implicit def longToFraction(n: Long): Fraction = Fraction(n, 1)

    implicit val fractionIsNumeric: Numeric[Fraction] = new Numeric[Fraction] {
      override def plus(x: Fraction, y: Fraction): Fraction = {
        val commonDenom = x.den * y.den
        val newNum = (x.num * y.den) + (y.num * x.den)
        Fraction(newNum, commonDenom).reduce
      }

      override def minus(x: Fraction, y: Fraction): Fraction = {
        val commonDenom = x.den * y.den
        val newNum = (x.num * y.den) - (y.num * x.den)
        Fraction(newNum, commonDenom).reduce
      }

      override def times(x: Fraction, y: Fraction): Fraction = Fraction(x.num * y.num, x.den * y.den).reduce

      override def negate(x: Fraction): Fraction = Fraction(-x.num, x.den)

      override def fromInt(x: Int): Fraction = Fraction(x, 1)

      override def toInt(x: Fraction): Int = (x.num / x.den).toInt

      override def toLong(x: Fraction): Long = (x.num / x.den).toLong

      override def toFloat(x: Fraction): Float = (x.num / x.den).toFloat

      override def toDouble(x: Fraction): Double = (x.num / x.den).toDouble

      override def compare(x: Fraction, y: Fraction): Int = {
        val xNum = x.num * y.den
        val yNum = y.num * x.den
        xNum.compareTo(yNum)
      }
    }
}
