package sbosley.euler.p101to150.p121to130.problem124

import sbosley.euler.util.math.Primes

object OrderedRadicals {

  private val max = 100000
  private val primes = Primes.primesToMax(max + 1000)

  def main(args: Array[String]): Unit = {
    val numbersWithRadicals = (1L, 1) :: (2 to max).map(n => (rad(n), n)).toList
    val sorted = numbersWithRadicals.sorted
    println(sorted(9999)._2)
  }

  private def rad(n: Int): Long = {
    if (n % 100 == 0) println(s"Processing $n")
    val primeFactors = Primes.primeFactors(n, Some(primes))
    primeFactors.keys.map(_.toLong).product
  }

}
