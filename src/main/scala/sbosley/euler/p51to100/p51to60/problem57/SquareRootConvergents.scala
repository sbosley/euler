package sbosley.euler.p51to100.p51to60.problem57

import sbosley.euler.util.math.MathHelpers.Fraction

object SquareRootConvergents {

  def main(args: Array[String]): Unit = {
    val expansions = (0 to 999).foldLeft(Map.empty[Int, Fraction]) { (memoized, iterations) =>
      memoized + (iterations -> sqrtFraction(iterations, memoized))
    }.mapValues(f => (f + 1).reduce)
    val count = expansions.values.count(f => f.num.toString.length > f.den.toString.length)
    println(count)
  }

  def sqrtFraction(iterations: Int, memoized: Map[Int, Fraction]): Fraction = {
    if (iterations == 0) Fraction(1, 2)
    else memoized.getOrElse(iterations, (sqrtFraction(iterations - 1, memoized) + 2).invert.reduce)
  }
}
