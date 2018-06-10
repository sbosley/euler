package sbosley.euler.p1to50.p21to30.problem27

import sbosley.euler.math.Primes

object QuadraticPrimes {

  private val maxPrime = 1000000
  private val primes = Primes.primesToMax(maxPrime).toSet

  def main(args: Array[String]): Unit = {
    val (maxPrimeSeq, (a, b)) = (for {
      b <- primes if b <= 1000
      a <- -1000 to 1000
    } yield {
      (maxNForPrimeSeq(a, b), (a, b))
    }).maxBy(_._1)
    println(s"Max prime seq: $maxPrimeSeq, a: $a, b: $b")
    println(s"ab = ${a * b}")
  }

  private def maxNForPrimeSeq(a: Int, b: Int): Int = {
    val p = polynomial(a, b) _
    Stream.from(0).find(n => {
      if (p(n) > 1000000) throw new IllegalStateException("p(n) got higher than our max prime")
      !primes(p(n))
    }).get - 1
  }

  private def polynomial(a: Int, b: Int)(n: Int): Int = {
    n * n + n * a + b
  }

}
