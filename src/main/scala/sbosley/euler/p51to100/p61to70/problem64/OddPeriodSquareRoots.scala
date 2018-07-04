package sbosley.euler.p51to100.p61to70.problem64

import sbosley.euler.math.MathHelpers
import sbosley.euler.math.MathHelpers.Fraction

import scala.annotation.tailrec

object OddPeriodSquareRoots {

  case class FractionalTerm(num: RootDifference, den: Int)
  case class RootDifference(root: RootTerm, diff: Int) {
    def complement: RootDifference = copy(diff = -diff)
    def timesComplement: Int = root.n - (diff * diff)
    def toDouble: Double = math.sqrt(root.n) + diff
  }
  case class RootTerm(n: Int)

  def main(args: Array[String]): Unit = {
    val oddPeriodCount = (2 to 10000).count(n => !MathHelpers.isSquare(n) && countPeriod(n) % 2 != 0)
    println(oddPeriodCount)
  }

  // a0 = floor sqrt(n)
  // remainder term = 1 / RootDifference(RootTerm(n), -floor(sqrt(n))
  // reduce remainder term to a1 + fractional component
  // When fractional component = RootDifference(RootTerm(n), -floor(sqrt(n))), terminate seq

  def countPeriod(n: Int): Int = {
    val a0 = math.sqrt(n).toInt
    val fractional0 = RootDifference(RootTerm(n), -a0)
    countPeriodRecursive(FractionalTerm(fractional0, 1), FractionalTerm(fractional0, 1), 0)
  }

  @tailrec
  private def countPeriodRecursive(originalFraction: FractionalTerm, currentFraction: FractionalTerm, countSoFar: Int): Int = {
    if (originalFraction == currentFraction && countSoFar > 0) countSoFar
    else countPeriodRecursive(originalFraction, nextPeriodicFraction(currentFraction), countSoFar + 1)
  }

  private def nextPeriodicFraction(term: FractionalTerm): FractionalTerm = {
    // (sqrt(n) - x) / y => y / (sqrt(n) - x) => y * (sqrt(n) + x) / (n - x^2)
    val newRationalPart = Fraction(term.den, term.num.timesComplement).reduce
    assert(newRationalPart.num == 1)
    val newDen = newRationalPart.den.toInt
    val newRootDiff = nextRootDiff(term.num.complement, newDen)
    FractionalTerm(newRootDiff, newDen)
  }

  @tailrec
  private def nextRootDiff(rootDiff: RootDifference, den: Int): RootDifference = {
    if (rootDiff.toDouble < den) rootDiff
    else nextRootDiff(rootDiff.copy(diff = rootDiff.diff - den), den)
  }
}
