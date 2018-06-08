package sbosley.euler.p1to20.problem12

import sbosley.euler.math.Primes

import scala.collection.mutable

object Problem12 {

  private val memoizedDivisors = mutable.Map[Int, Int]()

  def main(args: Array[String]): Unit = {
    val result = (2 until Int.MaxValue).find(triangleHas500Divisors).map(n => (BigInt(n) * BigInt(n + 1)) / 2)
    println(result)
  }

  def triangleHas500Divisors(n: Int): Boolean = {
    val nDiv = numDivisors(n)
    val nPlus1Div = numDivisors(n + 1)
    nDiv * nPlus1Div >= 500
  }

  private def numDivisors(n: Int): Int = {
    if (memoizedDivisors.contains(n)) memoizedDivisors(n)
    else {
      val divisors =
        if (n % 2 == 0) Primes.numDivisors(n / 2)
        else Primes.numDivisors(n)
      memoizedDivisors += n -> divisors
      divisors
    }
  }
}
