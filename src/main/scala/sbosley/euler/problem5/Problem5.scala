package sbosley.euler.problem5

import sbosley.euler.math.Primes

object Problem5 {

  def main(args: Array[String]): Unit = {
    println(productDivisibleBy1to20)
  }

  def productDivisibleBy1to20: Long = {
    Primes.primes[Long].takeWhile(_ <= 20).product * 8 * 3 // Primes from 1 - 20 + additional factors to get powers of primes
  }

}
