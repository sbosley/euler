package sbosley.euler.p1to50.p1to10.problem4

import sbosley.euler.string.StringExt._

object LargestPalindromeProduct {

  def main(args: Array[String]): Unit = {
    println(findMaxPalindromeProduct)
  }

  def findMaxPalindromeProduct: Int = {
    val products = for {
      i <- 1 to 999
      j <- 1 to 999
    } yield {
      i * j
    }
    products.filter(_.toString.isPalindrome).max
  }

}
