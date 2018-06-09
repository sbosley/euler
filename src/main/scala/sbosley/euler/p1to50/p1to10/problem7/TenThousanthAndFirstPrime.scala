package sbosley.euler.p1to50.p1to10.problem7

import sbosley.euler.math.Primes

object TenThousanthAndFirstPrime {

  def main(args: Array[String]): Unit = {
    println(Primes.primes[Long].apply(10000))
  }

}
