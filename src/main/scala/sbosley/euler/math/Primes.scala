package sbosley.euler.math

import Ordering.Implicits._
import Integral.Implicits._

object Primes {

  // Implements trial division -- not the most efficient
  def primes[T: Integral]: Stream[T] = {
    val integral = implicitly[Integral[T]]
    integral.fromInt(2) #:: Stream.iterate(integral.fromInt(3)) { _ + integral.fromInt(2)}.filter(isPrime)
  }

  private def isPrime[T : Integral](n: T): Boolean = {
    primes.takeWhile(p => p * p <= n).forall(n % _ != 0)
  }
}
