package sbosley.euler.p1to50.p21to30.problem21

import sbosley.euler.math.Primes

object Problem21 {

  def main(args: Array[String]): Unit = {
    val sumAmicables = (3 to 10000).foldLeft(Set.empty[Int]) { case (amicables, n) =>
      val d = sumProperDivisors(n)
      if (sumProperDivisors(d) == n && n != d) {
        amicables + n + d
      } else amicables
    }.filter(_ < 10000).sum
    println(sumAmicables)
  }

  private def sumProperDivisors(n: Int): Int = {
    Primes.properDivisors(n).sum
  }
}
