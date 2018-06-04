package sbosley.euler.problem6

import sbosley.euler.math.Sequences

object Problem6 {

  val N = 100

  def main(args: Array[String]): Unit = {
    println(squareOfSum(N) - sumOfSquares(N))
  }

  def sumOfSquares(n: Int): Int = {
    (1 to n map { x => x * x}).sum
  }

  def squareOfSum(n: Int): Int = {
    val sum = Sequences.sumToN(n)
    sum * sum
  }

}
