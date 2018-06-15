package sbosley.euler.p1to50.p41to50.problem44

import sbosley.euler.math.MathHelpers

object PentagonalNumbers {

  // Find the pair of pentagonal numbers, Pj and Pk, for which their sum and difference are pentagonal and
  // D = |Pk − Pj| is minimised; what is the value of D?
  // W.L.O.G. assume k > j
  // Find the smallest pentagonal number D that can be expresses as the difference between two greater pentagonals
  // Pj and Pk, where the Pj + Pk is also pentagonal.

  def main(args: Array[String]): Unit = {
    val thing = Stream.iterate(1L)(_ + 1)
      .find(i => {
        val D = pentagonal(i)
        // D = c(6n + 3c - 1) / 2
        val D2 = 2 * D
        val maxC = Stream.from(1).takeWhile(c => c * (3 * c + 5) <= 2 * D)
        maxC.exists(c => {
          // 6nc + 3c^2 - c = 2D
          // 6nc = 2D + c - 3c^2
          val x = D2 + c - 3 * c * c
          if (x > 0 && x % (6 * c) == 0) {
            val n = (D2 + c - 3 * c * c) / (6 * c)
            isPentagonal(pentagonal(n) + pentagonal(n + c))
          } else false
        })
      })

    thing.foreach(n => println(pentagonal(n)))
  }

  private def isPentagonal(n: Long): Boolean = {
    val maybeSolutions = MathHelpers.quadraticFormulaIntegers(3, -1, -2 * n)
    maybeSolutions.exists(x => x > 0 && pentagonal(x) == n)
  }

  private def pentagonal(x: Long): Long = x * (3 * x - 1) / 2

}
