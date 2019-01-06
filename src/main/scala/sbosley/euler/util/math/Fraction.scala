package sbosley.euler.util.math

import sbosley.euler.util.math.MathHelpers.gcd

case class Fraction(num: BigInt, den: BigInt) {

  override def toString: String = s"$num / $den"

  import Numeric.Implicits._
  def +(n: Int): Fraction = this + Fraction.intToFraction(n)
  def *(n: Int): Fraction = this * Fraction.intToFraction(n)
  def +(n: Long): Fraction = this + Fraction.longToFraction(n)
  def *(n: Long): Fraction = this * Fraction.longToFraction(n)
  def invert: Fraction = Fraction(den, num)
  def reduce: Fraction = {
    if (num < 0 && den < 0) {
      Fraction(num.abs, den.abs).reduce
    } else if (den < 0 && num > 0) {
      Fraction(-num, den.abs).reduce
    } else {
      val divisor = gcd(num.abs, den)
      Fraction(num / divisor, den / divisor)
    }
  }
}

object Fraction {

  implicit def intToFraction(n: Int): Fraction = Fraction(n, 1)
  implicit def longToFraction(n: Long): Fraction = Fraction(n, 1)

  def apply(n: Int): Fraction = Fraction(n, 1)
  def apply(n: Long): Fraction = Fraction(n, 1)

  def plus(x: Fraction, y: Fraction, reduce: Boolean): Fraction = {
    val commonDenom = x.den * y.den
    val newNum = (x.num * y.den) + (y.num * x.den)
    val rawResult = Fraction(newNum, commonDenom)
    if (reduce) rawResult.reduce else rawResult
  }

  def minus(x: Fraction, y: Fraction, reduce: Boolean): Fraction = {
    val commonDenom = x.den * y.den
    val newNum = (x.num * y.den) - (y.num * x.den)
    val rawResult = Fraction(newNum, commonDenom)
    if (reduce) rawResult.reduce else rawResult
  }

  def times(x: Fraction, y: Fraction, reduce: Boolean): Fraction = {
    val rawResult = Fraction(x.num * y.num, x.den * y.den)
    if (reduce) rawResult.reduce else rawResult
  }

  implicit val fractionIsNumeric: Numeric[Fraction] = new Numeric[Fraction] {

    override def plus(x: Fraction, y: Fraction): Fraction = Fraction.plus(x, y, reduce = true)

    override def minus(x: Fraction, y: Fraction): Fraction = Fraction.minus(x, y, reduce = true)

    override def times(x: Fraction, y: Fraction): Fraction = Fraction.times(x, y, reduce = true)

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
