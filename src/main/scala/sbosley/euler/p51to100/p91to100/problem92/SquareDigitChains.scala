package sbosley.euler.p51to100.p91to100.problem92

import scala.annotation.tailrec

object SquareDigitChains {

  def main(args: Array[String]): Unit = {
    val countConvergences = (1 until 10000000).foldLeft(Map.empty[Int, Boolean]) { (memoized, n) =>
      val converges = squareDigitChainConvergesTo89(n, memoized)
      memoized + (n -> converges)
    }.count(_._2)
    println(countConvergences)
  }

  @tailrec
  private def squareDigitChainConvergesTo89(n: Int, memoized: Map[Int, Boolean]): Boolean = {
    if (memoized.contains(n)) memoized(n)
    else if (n == 1) false
    else if (n == 89) true
    else {
      val squareDigitSum = n.toString.map(_.asDigit).map(x => x * x).sum
      squareDigitChainConvergesTo89(squareDigitSum, memoized)
    }
  }

}
