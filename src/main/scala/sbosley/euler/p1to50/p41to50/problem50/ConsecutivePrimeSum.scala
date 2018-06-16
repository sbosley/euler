package sbosley.euler.p1to50.p41to50.problem50

import sbosley.euler.math.Primes

object ConsecutivePrimeSum {

  private val max = 1000000
  private val primes = Primes.primesToMax(max)
  private val primesSum = primes.scanLeft(0L) { (sum, p) => sum + p }.tail.zipWithIndex
  private val primesSumMap = primesSum.toMap

  def main(args: Array[String]): Unit = {
    val max = primes.zipWithIndex.maxBy { case (p, index) => primesSumLength(p, index) }
    println(max)
  }

  // This algorithm is slow. collectFirst is fast and gets the right answer, but I don't know if it's correct
  private def primesSumLength(p: Int, pIndex: Int): Int = {
    val primesSumSubsequenceLengths = primesSum.view.take(pIndex).collect {
      case (sum, index) if sum >= p =>
        if (sum == p) index
        else {
          val indexOfStartSum = primesSumMap.get(sum - p)
          indexOfStartSum.map(index - _).getOrElse(-1)
        }
    }
    if (primesSumSubsequenceLengths.isEmpty) -1 else primesSumSubsequenceLengths.max
  }

}
