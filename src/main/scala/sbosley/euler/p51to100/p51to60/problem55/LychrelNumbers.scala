package sbosley.euler.p51to100.p51to60.problem55

import sbosley.euler.util.string.StringExt._

import scala.annotation.tailrec

object LychrelNumbers {

  def main(args: Array[String]): Unit = {
    val count = (1 to 10000).count(isLychrelNumber)
    println(count)
  }

  private def isLychrelNumber(n: Int): Boolean = {
    isLychrelNumberRecursive(BigInt(n), 0)
  }

  @tailrec
  private def isLychrelNumberRecursive(n: BigInt, iterationCount: Int): Boolean = {
    if (iterationCount >= 50) true
    else {
      val nextIteration = n + BigInt(n.toString.reverse)
      if (nextIteration.toString.isPalindrome) false
      else isLychrelNumberRecursive(nextIteration, iterationCount + 1)
    }
  }

}
