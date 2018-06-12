package sbosley.euler.p1to50.p31to40.problem36

import sbosley.euler.string.StringExt._

object DoubleBasePalindromes {

  def main(args: Array[String]): Unit = {
    val doubleBasePalindromes = (1 to 999).flatMap(getDoubleBasePalindromes).toSet
    println(doubleBasePalindromes.sum)
  }

  private def getDoubleBasePalindromes(n: Int): Set[Int] = {
    val nMirror1 = (n.toString + n.toString.reverse).toInt
    val nMirror2 = (n.toString.substring(1).reverse + n.toString).toInt
    Set(n, nMirror1, nMirror2).filter(x => x.toString.isPalindrome && x.toBinaryString.isPalindrome)
  }

}
