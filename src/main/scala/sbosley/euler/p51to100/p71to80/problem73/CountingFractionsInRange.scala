package sbosley.euler.p51to100.p71to80.problem73

import sbosley.euler.util.math.Fraction

import scala.annotation.tailrec

object CountingFractionsInRange {

  // Farey sequences (https://en.wikipedia.org/wiki/Farey_sequence)
  // See https://en.wikipedia.org/wiki/Farey_sequence#Next_term for next term algorithm

  private val MAX_N = 12000

  def main(args: Array[String]): Unit = {
    val prev = reduceNeighbors(Fraction(1, 4), Fraction(1, 3))
    val count = countTermsFrom(prev, Fraction(1, 3), 0)
    println(count)
  }

  @tailrec
  private def reduceNeighbors(left: Fraction, right: Fraction): Fraction = {
    if (left.den + right.den > MAX_N) left
    else reduceNeighbors(Fraction(left.num + right.num, left.den + right.den), right)
  }

  @tailrec
  private def countTermsFrom(prev: Fraction, curr: Fraction, count: Long): Long = {
    val nextTerm = computeNextTerm(prev, curr)
    if (count % 10000 == 0) println(s"Count so far is $count")
    if (nextTerm == Fraction(1, 2)) count
    else countTermsFrom(curr, nextTerm, count + 1)
  }

  private def computeNextTerm(prev: Fraction, curr: Fraction): Fraction = {
    val a = prev.num
    val b = prev.den
    val c = curr.num
    val d = curr.den

    val p = ((MAX_N + b) / d) * c - a
    val q = ((MAX_N + b) / d) * d - b
    Fraction(p, q).reduce
  }
}
