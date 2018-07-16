package sbosley.euler.p51to100.p81to90.problem87

import sbosley.euler.util.math.Primes

object PrimePowerTriples {

  private val MAX = 50000000
  private val primes = Primes.primesToMax(100000)
  private val primeSquares = primes.map(p => p.toLong * p).takeWhile(_ < MAX)
  private val primeCubes = primes.map(p => p.toLong * p * p).takeWhile(_ < MAX)
  private val primeFourths = primes.map(p => p.toLong * p * p * p).takeWhile(_ < MAX)

  def main(args: Array[String]): Unit = {
    val allSumTriples =
      (for {
        fourth <- primeFourths
        cube <- primeCubes
        square <- primeSquares
      } yield {
        square + cube + fourth
      }).toSet
    println(allSumTriples.count(_ < MAX))
  }

}
