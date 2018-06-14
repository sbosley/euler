package sbosley.euler.math

import Integral.Implicits._

object MathHelpers {

  val FIBONACCI_STREAM: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: FIBONACCI_STREAM.zip(FIBONACCI_STREAM.tail).map { n => n._1 + n._2 }

  def sumToN[T : Integral](n: T): T = {
    val integral = implicitly[Integral[T]]
    (n * (n + integral.one)) / integral.fromInt(2)
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

  def factorial(n: Long): Long = {
    if (n == 0 || n == 1) 1
    else n * factorial(n - 1)
  }

}
