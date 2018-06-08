package sbosley.euler.p1to20.problem20

object Problem20 {

  def main(args: Array[String]): Unit = {
    println(sumFactorialDigits(100))
  }

  def sumFactorialDigits(n: Int): Long = {
    val factorial = (1 to n).foldLeft(BigInt(1)) { (product, i) =>
      product * i
    }
    factorial.toString.toCharArray.map(_.asDigit).sum
  }

}
