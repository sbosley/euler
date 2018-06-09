package sbosley.euler.p1to50.p11to20.problem20

object FactorialDigitSum {

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
