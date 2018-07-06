package sbosley.euler.util.math

import Ordering.Implicits._
import Integral.Implicits._
import scala.annotation.tailrec
import scala.collection.mutable

object Primes {

  def primesToMax(max: Int): List[Int] = {
    // Indices representing (1, 3, 5, 7, 9, ...)
    val indices = mutable.ArrayBuffer.fill((max + 1) / 2)(1)

    val intSqrt = math.sqrt(max).toInt
    for (i <- 3 to intSqrt by 2) {
      for (nonPrime <- i * i to max by 2 * i) {
        indices.update(nonPrime / 2, 0)
      }
    }

    val rest = for (i <- indices.indices if indices(i) == 1) yield 2 * i + 1
    if (rest.isEmpty) List(2, 3)
    else 2 :: rest.tail.toList
  }

  def allDivisors(n: Int, cachedPrimes: Option[List[Int]] = None): Set[Int] = {
    val divisors = Set(1)
    val primes = cachedPrimes.getOrElse(primesToMax(n))
    computeDivisorsRecursive(n, primes, divisors)
  }

  def properDivisors(n: Int): Set[Int] = {
    allDivisors(n) - n
  }

  def primeFactors(n: Int, cachedPrimes: Option[List[Int]] = None): Map[Int, Int] = {
    val primes = cachedPrimes.getOrElse(primesToMax(n))
    computePrimeFactorsRecursive(n, primes, Map.empty)
  }

  @tailrec
  private def computePrimeFactorsRecursive(n: Int, primes: List[Int], primeFactorization: Map[Int, Int]): Map[Int, Int] = {
    val p = primes.head
    if (n == 0 || n == 1) primeFactorization
    else if (n % p == 0) {
      val (newN, primeFactorCount) = countDDividesN(n, p, 0)
      computePrimeFactorsRecursive(newN, primes.tail, primeFactorization + (p -> primeFactorCount))
    } else computePrimeFactorsRecursive(n, primes.tail, primeFactorization)
  }

  @tailrec
  private def computeDivisorsRecursive(n: Int, primes: List[Int], divisors: Set[Int]): Set[Int] = {
    val p = primes.head
    if (n == 0 || n == 1) divisors
    else if (n % p == 0) computeDivisorsRecursive(n / p, primes, divisors ++ divisors.map(_ * p))
    else computeDivisorsRecursive(n, primes.tail, divisors)
  }

  def numDivisors(n: Int): Int = {
    numDivisorsRecursive(n, 2 #:: Stream.from(3, 2), 1)
  }

  @tailrec
  private def numDivisorsRecursive(n: Int, divisorsToCheck: Seq[Int], divisors: Int): Int = {
    if (n == 1) divisors
    else {
      val d = divisorsToCheck.head
      val (newN, count) = countDDividesN(n, d)
      numDivisorsRecursive(newN, divisorsToCheck.tail, divisors * (count + 1))
    }
  }

  @tailrec
  def countDDividesN(n: Int, d: Int, count: Int = 0): (Int, Int) = {
    if (n % d != 0) (n, count)
    else countDDividesN(n / d, d, count + 1)
  }

  // Implements trial division -- not the most efficient
  def primes[T: Integral]: Stream[T] = {
    val integral = implicitly[Integral[T]]
    integral.fromInt(2) #:: Stream.iterate(integral.fromInt(3)) { _ + integral.fromInt(2)}.filter(isPrime)
  }

  private def isPrime[T : Integral](n: T): Boolean = {
    primes.takeWhile(p => p * p <= n).forall(n % _ != 0)
  }
}
