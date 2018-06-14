package sbosley.euler.p1to50.p41to50.problem43

object SubstringDivisibility {

  private val primes = List(17, 13, 11, 7, 5, 3, 2)
  private val chars = "0123456789".toSet

  def main(args: Array[String]): Unit = {
    val lastDigitCandidates = (102 to 999 by 17)
      .filter(_.toString.toSet.size == 3)
      .map(_.toString)
    val allCandidates = lastDigitCandidates.foldLeft(Set.empty[String]) { (set, digits) =>
      findSubstringPandigitals(digits, primes.tail, set)
    }.filter(i => i.toString.length == chars.size && i.toString.toSet == chars)
    println(allCandidates.map(_.toLong).sum)
  }

  private def findSubstringPandigitals(digitsSoFar: String, remainingPrimes: List[Int], solutions: Set[String]): Set[String] = {
    val digits = digitsSoFar.toString
    val availableChars = chars -- digits.toSet
    if (remainingPrimes.isEmpty) {
      solutions + (availableChars.toList.head.toString + digits)
    } else {
      val nextPrime = remainingPrimes.head
      val nextTwoDigits = digits.substring(0, 2)
      val nextDigitCandidates = availableChars
        .filter(c => (c + nextTwoDigits).toInt % nextPrime == 0)
        .map(c => c + digits)
      if (nextDigitCandidates.isEmpty) solutions
      else {
        solutions ++ nextDigitCandidates.foldLeft(Set.empty[String]) { (set, nextDigits) =>
          findSubstringPandigitals(nextDigits, remainingPrimes.tail, set)
        }
      }
    }
  }

}
