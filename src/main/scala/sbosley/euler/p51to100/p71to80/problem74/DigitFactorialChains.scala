package sbosley.euler.p51to100.p71to80.problem74

import sbosley.euler.util.math.MathHelpers

import scala.annotation.tailrec

object DigitFactorialChains {

  private val factorial: Map[Int, BigInt] = (0 to 9).map(n => n -> MathHelpers.factorial(n))(collection.breakOut)

  def main(args: Array[String]): Unit = {
    println((1 until 1000000).count(n => {
      if (n % 1000 == 0) println(s"Processing $n")
      countNonRepeatingTerms(n) == 60
    }))
  }

  def countNonRepeatingTerms(n: BigInt): Int = countNonRepeatingTerms(n, Set(n))

  @tailrec
  def countNonRepeatingTerms(n: BigInt, seenTerms: Set[BigInt]): Int = {
    val sum = digitFactorialSum(n)
    if (seenTerms.contains(sum)) seenTerms.size
    else countNonRepeatingTerms(sum, seenTerms + sum)
  }

  def digitFactorialSum(n: BigInt): BigInt = {
    n.toString.map(d => factorial(d.asDigit)).sum
  }

}
