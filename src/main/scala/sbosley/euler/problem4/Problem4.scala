package sbosley.euler.problem4

import sbosley.euler.string.StringExt._

object Problem4 {

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
