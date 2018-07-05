package sbosley.euler.p51to100.p61to70.problem70

import sbosley.euler.util.math.{Fraction, MathHelpers, Primes}
import sbosley.euler.util.string.StringExt._

import scala.collection.mutable

object TotientPermutation {

  private val MAX_N = 10000000
  private val primes = Primes.primesToMax(MAX_N + 100)

  private val cachedTotients = mutable.Map[Int, Int]()
  primes.foreach(p => cachedTotients += p -> (p - 1))
  (1 to MAX_N / 10).foreach(n => cachedTotients += n -> MathHelpers.totient(n, Some(primes)))

  def main(args: Array[String]): Unit = {
    val result = (2 until MAX_N).map(n => {
      Fraction(n, fastTotient(n))
    }).filter(f => f.num.toString.isPermutation(f.den.toString)).min.num
    println(result)
  }

  private def fastTotient(n: Int): Int = {
    cachedTotients.getOrElseUpdate(n, {
      val divisors = findRelativelyPrimeDivisors(n)
      divisors.fold(
        { onePrime => (Fraction(onePrime - 1, onePrime) * n).reduce.num.toInt },
        { case (divisor1, divisor2) => fastTotient(divisor1) * fastTotient(divisor2)})
    })
  }

  private def findRelativelyPrimeDivisors(n: Int): Either[Int, (Int, Int)] = {
    findRelativelyPrimeDivisorsRecursive(n, n, primes, 1)
  }

  private def findRelativelyPrimeDivisorsRecursive(original: Int, remaining: Int, primes: List[Int], divisor: Int): Either[Int, (Int, Int)] = {
    if (cachedTotients.contains(remaining)) Right((remaining, divisor))
    else if (remaining % primes.head != 0) findRelativelyPrimeDivisorsRecursive(original, remaining, primes.tail, divisor)
    else {
      val (newRemaining, pCount) = Primes.countDDividesN(remaining, primes.head)
      val primeDivisor = math.pow(primes.head, pCount).toInt
      if (primeDivisor == original) Left(primes.head)
      else findRelativelyPrimeDivisorsRecursive(original, newRemaining, primes, divisor * primeDivisor)
    }
  }

}
