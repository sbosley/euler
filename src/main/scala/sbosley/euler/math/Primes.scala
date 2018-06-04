package sbosley.euler.math

import Ordering.Implicits._
import Integral.Implicits._
import scala.collection.mutable

object Primes {

  def primesToMax(max: Int): Seq[Int] = {
    // Indices representing (1, 3, 5, 7, 9, ...)
    val indices = mutable.ArrayBuffer.fill((max + 1) / 2)(1)

    val intSqrt = math.sqrt(max).toInt
    for (i <- 3 to intSqrt by 2) {
      for (nonPrime <- i * i to max by 2 * i) {
        indices.update(nonPrime / 2, 0)
      }
    }

    2 :: (for (i <- indices.indices if indices(i) == 1) yield 2 * i + 1).tail.toList
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
