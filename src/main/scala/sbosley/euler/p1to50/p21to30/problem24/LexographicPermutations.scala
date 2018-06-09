package sbosley.euler.p1to50.p21to30.problem24

import sbosley.euler.math.MathHelpers

import scala.annotation.tailrec

object LexographicPermutations {

  val digits = List("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")

  def main(args: Array[String]): Unit = {
    println(computeNthPermutation(1000000))
  }

  def computeNthPermutation(n: Int): String = {
    nthPermutationRecursive(n, "", 0)
  }

  @tailrec
  private def nthPermutationRecursive(n: Int, digitsUsed: String, permutationsSoFar: Long): String = {
    if (digitsUsed.length == digits.length) digitsUsed
    else {
      val remainingDigits = digits.filterNot(digitsUsed.contains)
      // Next digit is at index s.t. (i + 1) * remainingPermsCount + remainingPerms > n
      val permutationsLeftAfterNextDigit = MathHelpers.factorial(remainingDigits.length - 1)
      val nextDigitIndex = remainingDigits.indices
        .find(i => (i + 1) * permutationsLeftAfterNextDigit + permutationsSoFar >= n)
        .getOrElse(0)
      val newPermutationCount = permutationsSoFar + (nextDigitIndex * permutationsLeftAfterNextDigit)
      nthPermutationRecursive(n, digitsUsed + remainingDigits(nextDigitIndex), newPermutationCount)
    }
  }

}
