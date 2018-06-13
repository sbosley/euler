package sbosley.euler.p1to50.p31to40.problem40

import scala.annotation.tailrec

object ChampernownesConstant {

  private val numTerms = 7

  def main(args: Array[String]): Unit = {
    val digits = findPowersOf10DigitsRecursive(Stream.from(1), 0, List.empty, 1)
    println(digits.product)
  }

  @tailrec
  private def findPowersOf10DigitsRecursive(ints: Stream[Int], digitCount: Int, result: List[Int], nextPow10: Int): List[Int] = {
    if (result.length >= numTerms) result
    else {
      val nextInt = ints.head.toString
      val newDigitCount = digitCount + nextInt.length
      if (digitCount < nextPow10 && newDigitCount >= nextPow10) {
        val nextDigitIndex = nextInt.indices.find { _ + digitCount + 1 == nextPow10 }.get
        val nextDigit = nextInt.charAt(nextDigitIndex).asDigit
        findPowersOf10DigitsRecursive(ints.tail, newDigitCount, nextDigit :: result, nextPow10 * 10)
      } else {
        findPowersOf10DigitsRecursive(ints.tail, newDigitCount, result, nextPow10)
      }
    }
  }

}
