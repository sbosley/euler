package sbosley.euler.p201to250.p201to210.problem206

import sbosley.euler.util.math.MathHelpers

object ConcealedSquare {

  // 1_2_3_4_5_6_7_8_9_0 is square
  // 1A2B3C4D5E6F7G8H900 is square
  // Iterate through all possible solutions, since there are only 10^8 of them. Takes a little over a minute.

  def main(args: Array[String]): Unit = {
    val result =
      (for {
        a <- Stream.range(0, 10, 1)
        b <- Stream.range(0, 10, 1)
        c <- Stream.range(0, 10, 1)
        d <- Stream.range(0, 10, 1)
        e <- Stream.range(0, 10, 1)
        f <- Stream.range(0, 10, 1)
        g <- Stream.range(0, 10, 1)
        h <- Stream.range(0, 10, 1)
      } yield {
        getNumberToTest(a, b, c, d, e, f, g, h)
      }).find(MathHelpers.isSquare)
    println(result)
    println(result.map(n => math.sqrt(n).toLong))
  }

  private def getNumberToTest(a: Long, b: Long, c: Long, d: Long, e: Long, f: Long, g: Long, h: Long): Long = {
    val base = 1020304050607080900L
    val result = base + 1000L * h + 100000L * g + 10000000L * f + 1000000000L * e + 100000000000L * d + 10000000000000L * c + 1000000000000000L * b + 100000000000000000L * a
    result
  }

}
