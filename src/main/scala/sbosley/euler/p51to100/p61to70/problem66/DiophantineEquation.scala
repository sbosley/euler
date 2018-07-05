package sbosley.euler.p51to100.p61to70.problem66

import sbosley.euler.util.math.{MathHelpers, PeriodicFractions}
import sbosley.euler.util.math.MathHelpers.Fraction

object DiophantineEquation {

  // From https://en.wikipedia.org/wiki/Pell%27s_equation#Fundamental_solution_via_continued_fractions
  // Pell's equation: x^2 - n * y^2 = 1
  // Let h_i / k_i denote the sequence of convergents to the regular continued fraction for
  // sqrt(n). This sequence is unique. Then the pair (x1,y1) solving Pell's equation and minimizing x satisfies x1 = hi
  // and y1 = ki for some i. This pair is called the fundamental solution. Thus, the fundamental solution may be found
  // by performing the continued fraction expansion and testing each successive convergent until a solution to Pell's
  // equation is found.

  def main(args: Array[String]): Unit = {
    val maxD = (2 to 1000).filterNot(MathHelpers.isSquare(_)).maxBy(d => findMinimalSolution(d).num)
    println(maxD)
  }

  def findMinimalSolution(n: Int): Fraction = {
    val convergent = PeriodicFractions.periodicSqrt(n)
    Stream.from(1).map(i => convergent.evalToPrecision(i)).find(f => solvesDiophantine(n, f)).get
  }

  private def solvesDiophantine(n: Int, f: MathHelpers.Fraction): Boolean = {
    f.num * f.num - n * f.den * f.den == 1
  }

}
