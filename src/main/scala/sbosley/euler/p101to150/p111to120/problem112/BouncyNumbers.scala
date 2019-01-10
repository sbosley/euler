package sbosley.euler.p101to150.p111to120.problem112

import sbosley.euler.util.SeqExt._
import sbosley.euler.util.math.Fraction

object BouncyNumbers {

  def main(args: Array[String]): Unit = {
    val targetPercentage = Fraction(99, 100)
    val bouncyProportions = Stream.from(1).scanLeft((0, 0)) { case ((bouncyCount, _), n) =>
      if (isBouncy(n)) {
        (bouncyCount + 1, n)
      } else {
        (bouncyCount, n)
      }
    }.tail
    val result = bouncyProportions.find { case (bouncyCount, n) => Fraction(bouncyCount, n).reduce == targetPercentage }
    println(result.get._2)
  }

  private def isBouncy(n: Int): Boolean = {
    val digitSeq = n.toString.toSeq
    !digitSeq.isSorted && !digitSeq.isReverseSorted
  }

}
