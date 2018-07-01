package sbosley.euler.p1to50.p41to50.problem45

import sbosley.euler.math.MathHelpers

object TriangularPentagonalHexagonal {

  def main(args: Array[String]): Unit = {
    // T285 = P165 = H143 = 40755
    val thing = Stream.iterate(144L)(_ + 1).find(n => {
      isPentagonal(hexagonal(n)) && isTriangular(hexagonal(n))
    })
    println(thing.map(hexagonal))
  }

  private def isHexagonal(n: Long): Boolean = {
    val maybeSolutions = MathHelpers.quadraticFormulaIntegers(2, -1, -n)
    maybeSolutions.exists(x => hexagonal(x) == n)
  }

  private def isPentagonal(n: Long): Boolean = {
    val maybeSolutions = MathHelpers.quadraticFormulaIntegers(3, -1, -2 * n)
    maybeSolutions.exists(x => x > 0 && pentagonal(x) == n)
  }

  private def isTriangular(n: Long): Boolean = {
    val maybeSolutions = MathHelpers.quadraticFormulaIntegers(1, 1, -2 * n)
    maybeSolutions.exists(x => triangular(x) == n)
  }

  private def triangular(x: Long): Long = MathHelpers.triangleNumber(x)
  private def pentagonal(x: Long): Long = x * (3 * x - 1) / 2
  private def hexagonal(x: Long): Long = x * (2 * x - 1)

}
