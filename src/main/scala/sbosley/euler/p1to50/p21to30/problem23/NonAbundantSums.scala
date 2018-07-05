package sbosley.euler.p1to50.p21to30.problem23

import sbosley.euler.util.math.Primes

import scala.annotation.tailrec

object NonAbundantSums {

  def main(args: Array[String]): Unit = {
    val abundantList = (1 to 28123)
      .filter(n => Primes.properDivisors(n).sum > n).toList
    val abundantSet = abundantList.toSet

    val sum = (1 to 28123).filter(cannotBeExpressedAsAbundantSum(_, abundantList, abundantSet)).map(_.toLong).sum
    println(sum)
  }

  @tailrec
  def cannotBeExpressedAsAbundantSum(n: Int, abundantList: List[Int], abundantSet: Set[Int]): Boolean = {
    if (abundantList.isEmpty) true
    else {
      val abundantToCheck = abundantList.head
      if (n <= abundantToCheck) true
      else if (abundantSet(n - abundantToCheck)) false
      else cannotBeExpressedAsAbundantSum(n, abundantList.tail, abundantSet)
    }
  }

}
