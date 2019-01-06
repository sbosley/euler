package sbosley.euler.p101to150.p121to130.problem125

import sbosley.euler.util.string.StringExt.StringExt

object PalindromicSums {

  private val squares = (1L to 10000L).map(n => n * n)
  private val sumSquares = squares.scanLeft(0L) { _ + _ }.tail
//  private val sumSquaresWithIndex = sumSquares.zipWithIndex
  private val squaresSet = squares.toSet
  private val sumSquaresSet = sumSquares.toSet

  def main(args: Array[String]): Unit = {
    val palindromicInRange = (2L to 100000000L).filter(_.toString.isPalindrome)
    val result = palindromicInRange.filter(isSumOfSquares).sum
    println(result)
  }

  private def isSumOfSquares(n: Long): Boolean = {
    !squaresSet(n) && sumSquares.exists(s => s == n || s > n && sumSquaresSet(s - n))
  }

//  private def isSumOfSquaresDebug(n: Long): Boolean = {
//    // if there exists a pair in sum squares such that s2 - s1 = n
//    val result = sumSquares.find(s => s == n || s > n && sumSquaresSet(s - n))
//    result.foreach(s => {
//      val s2Idx = sumSquaresWithIndex.find(_._1 == s).get._2 + 1
//      val s1Idx = sumSquaresWithIndex.find(_._1 == s - n).map(_._2 + 2).getOrElse(1)
//      val rangeSum = (s1Idx to s2Idx).map(x => x * x).sum
//      println(s"$s1Idx^2 + ... + $s2Idx^2 was $rangeSum, expected $n")
//    })
//    result.isDefined
//  }

}
