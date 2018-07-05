package sbosley.euler.p1to50.p41to50.problem47

import sbosley.euler.util.math.Primes

object DistinctPrimesFactors {

  private val primes = Primes.primesToMax(1000000)

  def main(args: Array[String]): Unit = {
    val fourDistinctFactors = (1 to 1000000).find(n => {
      Primes.primeFactors(n, Some(primes)).size >= 4 &&
      Primes.primeFactors(n + 1, Some(primes)).size >= 4 &&
      Primes.primeFactors(n + 2, Some(primes)).size >= 4 &&
      Primes.primeFactors(n + 3, Some(primes)).size >= 4
    })
    fourDistinctFactors.foreach(n => {
      println(s"$n => ${Primes.primeFactors(n, Some(primes))}")
      println(s"${n + 1} => ${Primes.primeFactors(n + 1, Some(primes))}")
      println(s"${n + 2} => ${Primes.primeFactors(n + 2, Some(primes))}")
      println(s"${n + 3} => ${Primes.primeFactors(n + 3, Some(primes))}")
    })
  }

}
