package sbosley.euler.p51to100.p61to70.problem64

import sbosley.euler.util.math.{MathHelpers, PeriodicFractions}

object OddPeriodSquareRoots {

  def main(args: Array[String]): Unit = {
    val oddPeriodCount = (2 to 10000).count(n => !MathHelpers.isSquare(n) &&
      PeriodicFractions.periodicSqrt(n).periodLength.getOrElse(0) % 2 != 0)
    println(oddPeriodCount)
  }
}
