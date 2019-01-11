package sbosley.euler.p101to150.p121to130.problem127

import sbosley.euler.util.Memoize
import sbosley.euler.util.math.Primes
import sbosley.euler.util.math.MathHelpers.gcd

object ABCHits {

  private val max = 120000
  private val primes = Primes.primesToMax(max + 100)

  private val memoizedPrimeFactorProducts = Memoize { n: Int =>
    Primes.primeFactors(n, Some(primes)).keySet.map(_.toLong).product
  }

  def main(args: Array[String]): Unit = {
    val hits = for {
      c <- 3 until max
      b <- c / 2 until c // since a < b, b cannot be < c / 2
      a = c - b // ensures a + b = c
      if isABCHit(a, b, c)
    } yield {
      (a, b, c)
    }
    val result = hits.map(_._3.toLong).sum
    println(result)
  }

  private def isABCHit(a: Int, b: Int, c: Int): Boolean = {
    if (c % 5000 == 0 && b == c / 2) println(s"Processing $c")
    a < b && rad(a, b, c) < c && gcd(a, b) == 1 && gcd(b, c) == 1 && gcd(a, c) == 1
  }

  private def rad(a: Int, b: Int, c: Int): Long = {
    val cPrimeFactorProduct = memoizedPrimeFactorProducts(c)
    if (cPrimeFactorProduct < c) {
      val bPrimeFactorProduct = memoizedPrimeFactorProducts(b)
      if (cPrimeFactorProduct * bPrimeFactorProduct < c) {
        val aPrimeFactorProduct = memoizedPrimeFactorProducts(a)
        aPrimeFactorProduct * bPrimeFactorProduct * cPrimeFactorProduct
      } else {
        Long.MaxValue
      }
    } else {
      Long.MaxValue
    }
  }

}
