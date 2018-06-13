package sbosley.euler.p1to50.p31to40.problem37

import sbosley.euler.math.Primes

object TruncatablePrimes {

  private val primes = Primes.primesToMax(1000000).toSet
  private val exceptions = Set(2, 3, 5, 7)

  def main(args: Array[String]): Unit = {
    val truncatablePrimes = primes.filter(p => isTruncatablePrime(p) && !exceptions(p))
    if (truncatablePrimes.size != 11) throw new IllegalArgumentException("Need more primes")
    println(truncatablePrimes.sum)
  }

  private def isTruncatablePrime(n: Int): Boolean = {
    val nStr = n.toString
    (1 until nStr.length).forall(nDigits =>
      primes(nStr.substring(0, nStr.length - nDigits).toInt) &&
      primes(nStr.substring(nDigits).toInt)
    )
  }

}
