package sbosley.euler.p1to50.p1to10.problem2

import sbosley.euler.util.math.MathHelpers

object EvenFibonacciNums {

  def main(args: Array[String]): Unit = {
    println(sumEvenFibs)
  }

  def sumEvenFibs: BigInt = {
    MathHelpers.FIBONACCI_STREAM.filter( _ % 2 == 0).takeWhile(_ <= 4000000).sum
  }
}
