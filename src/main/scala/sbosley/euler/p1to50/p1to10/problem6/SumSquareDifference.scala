package sbosley.euler.p1to50.p1to10.problem6

import sbosley.euler.util.math.MathHelpers

object SumSquareDifference {

  val N = 100

  def main(args: Array[String]): Unit = {
    println(squareOfSum(N) - sumOfSquares(N))
  }

  def sumOfSquares(n: Int): Int = {
    (1 to n map { x => x * x}).sum
  }

  def squareOfSum(n: Int): Int = {
    val sum = MathHelpers.triangleNumber(n)
    sum * sum
  }

}
