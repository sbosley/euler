package sbosley.euler.p51to100.p91to100.problem95

import sbosley.euler.util.Memoize
import sbosley.euler.util.math.Primes

import scala.annotation.tailrec

object AmicableChains {

  private val MAX = 1000000
  private val primes = Primes.primesToMax(MAX)
  private val primesSet = primes.toSet

  def main(args: Array[String]): Unit = {
    val result = (1 to MAX)
      .flatMap(n => getAmicableChain(n, n, Set(n)))
      .maxBy(_.size)
      .min
    println(result)
  }

  private val divisorSum = Memoize { (n: Int) => Primes.properDivisors(n, Some(primes)).sum }

  @tailrec
  private def getAmicableChain(n: Int, originalItem: Int, elems: Set[Int]): Option[Set[Int]] = {
    if (primesSet(n)) None
    else {
      val nextItem = divisorSum(n)
      if (nextItem == originalItem) Some(elems)
      else if (nextItem > MAX || elems(nextItem)) None
      else getAmicableChain(nextItem, originalItem, elems + nextItem)
    }
  }

}
