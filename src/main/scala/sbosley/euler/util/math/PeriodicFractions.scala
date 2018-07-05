package sbosley.euler.util.math

import sbosley.euler.util.math.MathHelpers.Fraction

import scala.annotation.tailrec

object PeriodicFractions {

  // a0 = floor sqrt(n)
  // remainder term = 1 / RootDifference(RootTerm(n), -floor(sqrt(n))
  // reduce remainder term to a1 + fractional component
  // When fractional component = RootDifference(RootTerm(n), -floor(sqrt(n))), terminate seq

  case class FractionalTerm(num: RootDifference, den: Int)
  case class RootDifference(root: RootTerm, diff: Int) {
    def complement: RootDifference = copy(diff = -diff)
    def timesComplement: Int = root.n - (diff * diff)
    def toDouble: Double = math.sqrt(root.n) + diff
  }
  case class RootTerm(n: Int)
  case class ConvergentFraction(base: Int, period: Stream[Int], periodLength: Option[Int]) {

    def evalToPrecision(n: Int): Fraction = {
      period.take(n).foldRight(Fraction(1, 0)) { (i, fraction) =>
        fraction.invert + i
      }.invert + base
    }
  }

  def periodicSqrt(n: Int): ConvergentFraction = {
    val a0 = math.sqrt(n).toInt
    val fractional0 = RootDifference(RootTerm(n), -a0)
    val periodSeq = computePeriodRecursive(
      FractionalTerm(fractional0, 1),
      FractionalTerm(fractional0, 1),
      Seq.empty)
    ConvergentFraction(a0, Stream.continually(periodSeq).flatten, Some(periodSeq.size))
  }

  @tailrec
  private def computePeriodRecursive(originalFraction: FractionalTerm, currentFraction: FractionalTerm, acc: Seq[Int]): Seq[Int] = {
    if (originalFraction == currentFraction && acc.nonEmpty) acc
    else {
      val (wholePart, nextPeriodic) = nextPeriodicFraction(currentFraction)
      computePeriodRecursive(originalFraction, nextPeriodic, acc :+ wholePart)
    }
  }

  private def nextPeriodicFraction(term: FractionalTerm): (Int, FractionalTerm) = {
    // (sqrt(n) - x) / y => y / (sqrt(n) - x) => y * (sqrt(n) + x) / (n - x^2)
    val newRationalPart = Fraction(term.den, term.num.timesComplement).reduce
    assert(newRationalPart.num == 1)
    val newDen = newRationalPart.den.toInt
    val (wholePart, newRootDiff) = nextRootDiff(term.num.complement, newDen, 0)
    (wholePart, FractionalTerm(newRootDiff, newDen))
  }

  @tailrec
  private def nextRootDiff(rootDiff: RootDifference, den: Int, wholePart: Int): (Int, RootDifference) = {
    if (rootDiff.toDouble < den) (wholePart, rootDiff)
    else nextRootDiff(rootDiff.copy(diff = rootDiff.diff - den), den, wholePart + 1)
  }

}
