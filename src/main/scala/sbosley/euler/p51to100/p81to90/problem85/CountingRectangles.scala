package sbosley.euler.p51to100.p81to90.problem85

import sbosley.euler.util.math.MathHelpers

object CountingRectangles {

  def main(args: Array[String]): Unit = {
    // Subrectangle count is (sum to m) * (sum to n) (not hard to prove)
    // find m, n s.t. m * (m + 1) * n * (n + 1) / 4 is closest to 2M
    // if n == 1, max m is ~1500
    // W.L.O.G. assume m >= n
    case class Rectangle(m: Int, n: Int) {
      def subrectangleCount: Int = countRectangles(m, n)
      def area: Int = m * n
    }
    val possibleRectangles = for {
      m <- 1 to 1500
      n <- 1 to m
    } yield {
      Rectangle(m, n)
    }

    val closest = possibleRectangles.minBy(r => Math.abs(2000000 - r.subrectangleCount))
    println(closest)
    println(closest.subrectangleCount)
    println(closest.m * closest.n)
  }

  def countRectangles(m: Int, n: Int): Int = {
    MathHelpers.triangleNumber(m) * MathHelpers.triangleNumber(n)
  }

}
