package sbosley.euler.p351to400.p381to390.problem387

import scala.annotation.tailrec
import scala.collection.mutable

object HarshadNumbers {

  private val max = math.pow(10, 14).toLong
  private val primeCertainty = 100
  private val digits = Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  private val oddDigits = digits.filter(_ % 2 != 0)

  case class RightTruncatableHarshadNumber(n: Long, digitSum: Long)

  def main(args: Array[String]): Unit = {
    val seeds = digits.filter(_ > 0).map(n => RightTruncatableHarshadNumber(n, n))
    val result = buildUpSolutions(seeds)
    println(result.map(BigInt(_)).sum)
  }

  @tailrec
  private def buildUpSolutions(knownRightTruncatables: Seq[RightTruncatableHarshadNumber], generation: Int = 1, acc: mutable.Set[Long] = mutable.Set.empty): Set[Long] = {
    val nextRound = knownRightTruncatables.flatMap(buildMoreRightTruncatables)
    if (nextRound.isEmpty) acc.toSet // finished generation
    else {
      println(s"Processing generation $generation")
      val solutionsInRound = findSolutionsFromBase(nextRound)
      buildUpSolutions(nextRound, generation + 1, acc ++= solutionsInRound)
    }
  }

  private def buildMoreRightTruncatables(rthn: RightTruncatableHarshadNumber): Seq[RightTruncatableHarshadNumber] = {
    for {
      digit <- digits
      newN = rthn.n * 10 + digit
      newSum = rthn.digitSum + digit
      if newN < max && newN % newSum == 0
    } yield {
      RightTruncatableHarshadNumber(newN, newSum)
    }
  }

  private def findSolutionsFromBase(numbers: Seq[RightTruncatableHarshadNumber]): Set[Long] = {
    numbers.filter(isStrongHarshadNumber).flatMap(findPrimesForStrongRightTruncatableHarshadNumber)(collection.breakOut)
  }

  private def isStrongHarshadNumber(rthn: RightTruncatableHarshadNumber): Boolean = {
    BigInt(rthn.n / rthn.digitSum).isProbablePrime(primeCertainty)
  }

  private def findPrimesForStrongRightTruncatableHarshadNumber(srthn: RightTruncatableHarshadNumber): Seq[Long] = {
    for {
      digit <- oddDigits
      newN = srthn.n * 10 + digit
      if BigInt(newN).isProbablePrime(primeCertainty) && newN < max
    } yield {
      newN
    }
  }
}
