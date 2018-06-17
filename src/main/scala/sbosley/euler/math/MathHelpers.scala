package sbosley.euler.math

import Integral.Implicits._
import scala.annotation.tailrec
import scala.collection.mutable

object MathHelpers {

  val FIBONACCI_STREAM: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: FIBONACCI_STREAM.zip(FIBONACCI_STREAM.tail).map { n => n._1 + n._2 }

  def sumToN[T : Integral](n: T): T = {
    val integral = implicitly[Integral[T]]
    (n * (n + integral.one)) / integral.fromInt(2)
  }

  def factorial(n: Long): BigInt = {
    factorialRecursive(n, 1)
  }

  @tailrec
  def gcd[T : Integral](a: T, b: T): T = {
    if (b == implicitly[Integral[T]].zero) a
    else gcd(b, a % b)
  }

  def powerset[T](s: Set[T]): Set[Set[T]] = {
    val builder = s.foldLeft(mutable.Set[Set[T]]()) { (builder, item) =>
      builder.foldLeft(builder) { (b, subset) =>
        b += subset + item
      }
      builder += Set(item)
    }
    (builder += Set.empty).toSet
  }

  case class Fraction(num: BigInt, den: BigInt) extends Ordered[Fraction] {

    override def toString: String = s"$num / $den"

    def +(n: Int): Fraction = Fraction(n * den + num, den)
    def invert: Fraction = Fraction(den, num)
    def reduce: Fraction = {
      val divisor = gcd(num, den)
      Fraction(num / divisor, den / divisor)
    }

    override def compare(that: Fraction): Int = {
      val newNum = num * that.den
      val otherNewNum = that.num * den
      newNum.compareTo(otherNewNum)
    }
  }

  @tailrec
  private def factorialRecursive(n: Long, acc: BigInt): BigInt = {
    if (n == 1) acc
    else factorialRecursive(n - 1, acc * n)
  }

  // Finds solutions to an^2 + bn + c
  def quadraticFormula(a: Double, b: Double, c: Double): (Double, Double) = {
    // (-b +/- sqrt(b^2 - 4ac) / 2a)
    val sqrtTerm = Math.sqrt(b * b - 4 * a * c)
    ((-b + sqrtTerm) / (2 * a), (-b - sqrtTerm) / (2 * a))
  }

  // Finds integer solutions to an^2 + bn + c
  def quadraticFormulaIntegers(a: Long, b: Long, c: Long): Set[Long] = {
    val (sol1, sol2) = quadraticFormula(a, b, c)
    Set(sol1.toLong + 1, sol1.toLong, sol1.toLong - 1, sol2.toLong + 1, sol2.toLong, sol2.toLong - 1)
      .filter(x => a * x * x + b * x + c == 0)
  }
}
