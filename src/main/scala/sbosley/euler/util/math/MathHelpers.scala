package sbosley.euler.util.math

import Integral.Implicits._
import scala.annotation.tailrec
import scala.collection.mutable

object MathHelpers {

  val FIBONACCI_STREAM: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: FIBONACCI_STREAM.zip(FIBONACCI_STREAM.tail).map { n => n._1 + n._2 }

  def triangleNumber[T : Integral](n: T): T = {
    val integral = implicitly[Integral[T]]
    (n * (n + integral.one)) / integral.fromInt(2)
  }

  def squareNumber[T : Integral](n: T): T = {
    n * n
  }

  def pentagonalNumber[T : Integral](n: T): T = {
    val integral = implicitly[Integral[T]]
    (n * (integral.fromInt(3) * n - integral.one)) / integral.fromInt(2)
  }

  def hexagonalNumber[T : Integral](n: T): T = {
    val integral = implicitly[Integral[T]]
    n * (integral.fromInt(2) * n - integral.one)
  }

  def heptagonalNumber[T : Integral](n: T): T = {
    val integral = implicitly[Integral[T]]
    (n * (integral.fromInt(5) * n - integral.fromInt(3))) / integral.fromInt(2)
  }

  def octagonalNumber[T : Integral](n: T): T = {
    val integral = implicitly[Integral[T]]
    n * (integral.fromInt(3) * n - integral.fromInt(2))
  }

  def factorial(n: Long): BigInt = {
    factorialRecursive(n, 1)
  }

  @tailrec
  def gcd[T : Integral](a: T, b: T): T = {
    if (b == implicitly[Integral[T]].zero) a
    else gcd(b, a % b)
  }

  def lcm[T : Integral](a: T, b: T): T = {
    (a / gcd(a, b)) * b
  }

  def lcm[T : Integral](items: List[T]): T = {
    items.foldLeft(implicitly[Integral[T]].one) { (lcmAcc, n) =>
      lcm(lcmAcc, n)
    }
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

  @tailrec
  private def factorialRecursive(n: Long, acc: BigInt): BigInt = {
    if (n == 0 || n == 1) acc
    else factorialRecursive(n - 1, acc * n)
  }

  def isSquare(n: Long): Boolean = {
    quadraticFormulaIntegers(1L, 0L, -n).nonEmpty
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

  // Euler's product formula: phi(n) = n * (primeFactors(n).map(p => 1 - 1 / p)).product
  def totient(n: Int, cachedPrimes: Option[List[Int]] = None): Int = {
    val primeFactors = Primes.primeFactors(n, cachedPrimes)
    val fractions = primeFactors.keySet.map(p => Fraction(p - 1, p))
    totientFromProduct(n, fractions)
  }

  private def totientFromProduct(n: Int, fSeq: Set[Fraction]): Int = {
    import Numeric.Implicits._
    (fSeq.product * n).toInt()
  }
}
