package sbosley.euler.p1to50.p1to10.problem3

import sbosley.euler.util.math.Primes

import scala.annotation.tailrec

object LargestPrimeFactor {

  val NUM_TO_FACTOR = 600851475143L

  def main(args: Array[String]): Unit = {
    println(findMaxPrimeFactor(NUM_TO_FACTOR, Primes.primes[Long]))
  }

  @tailrec
  def findMaxPrimeFactor(num: Long, primes: Stream[Long]): Long = {
    val primeToCheck = primes.head
    if (num == primeToCheck) primeToCheck
    else {
      var reduced = num
      while (reduced % primeToCheck == 0) {
        reduced /= primeToCheck
      }
      findMaxPrimeFactor(reduced, primes.tail)
    }
  }

}
