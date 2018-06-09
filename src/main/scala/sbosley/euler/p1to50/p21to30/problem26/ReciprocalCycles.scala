package sbosley.euler.p1to50.p21to30.problem26

import scala.annotation.tailrec

object ReciprocalCycles {

  private val repeatingDecimalRegex = "0\\.\\d*\\((\\d+\\))".r

  def main(args: Array[String]): Unit = {
    val max = 2 until 1000 maxBy { d => getDecimalDigits(d) match {
      case repeatingDecimalRegex(sequence) => sequence.length
      case _ => println(s"Something went wrong for $d"); 0
    }}
    println(max)
  }

  private def getDecimalDigits(d: Int): String = {
    "0." + getDecimalDigitsRecursive(1, d, "", Map.empty)
  }

  @tailrec
  private def getDecimalDigitsRecursive(remainder: Int, d: Int, digits: String, seenRemainders: Map[Int, Int]): String = {
    val r10 = remainder * 10
    if (seenRemainders.contains(remainder)) {
      val indexOfRepeatSeq = seenRemainders(remainder)
      digits.substring(0, indexOfRepeatSeq) + "(" + digits.substring(indexOfRepeatSeq) + ")"
    }
    else if (r10 % d == 0) digits + (r10 / d)
    else {
      val nextDigit = r10 / d
      val nextRemainder = if (nextDigit == 0) r10 else r10 % d
      getDecimalDigitsRecursive(nextRemainder, d, digits + nextDigit, seenRemainders + (remainder -> digits.length))
    }
  }

}
