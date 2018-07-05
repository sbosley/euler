package sbosley.euler.p1to50.p1to10.problem5

import sbosley.euler.util.math.Primes

object SmallestMultiple {

  def main(args: Array[String]): Unit = {
    println(productDivisibleBy1to20)
  }

  def productDivisibleBy1to20: Long = {
    Primes.primes[Long].takeWhile(_ <= 20).product * 8 * 3 // Primes from 1 - 20 + additional factors to get powers of primes
  }

}
