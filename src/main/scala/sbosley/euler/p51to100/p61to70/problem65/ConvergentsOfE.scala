package sbosley.euler.p51to100.p61to70.problem65

import sbosley.euler.math.MathHelpers.Fraction

object ConvergentsOfE {

  def main(args: Array[String]): Unit = {
    val eFractions = 1 :: 2 :: Stream.from(2).flatMap(i => Seq(1, 1, 2 * i)).take(97).toList
    val convergent = eFractions.foldRight(Fraction(1, 0)) { (i, fraction) =>
      fraction.invert + i
    }.invert + 2
    println(convergent)
    println(convergent.num.toString.map(_.asDigit).sum)
  }

}
