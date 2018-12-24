package sbosley.euler.p51to100.p91to100.problem94

import sbosley.euler.util.math.{Fraction, MathHelpers}
import sbosley.euler.util.math.Fraction.longToFraction

object AlmostEquilateralTriangles {

  def main(args: Array[String]): Unit = {
    val perimeters =
      for {
        mainSide <- Range.Long(2, 333333334, 1)
        thirdSide <- Seq(mainSide + 1, mainSide - 1)
        perimeter = (mainSide * 2) + thirdSide
        if perimeter <= 1000000000L && hasIntegerArea(thirdSide, mainSide)
      } yield {
        perimeter
      }
    val perimetersSum = perimeters.map(BigInt(_)).sum
    println(perimetersSum)
  }

  private def hasIntegerArea(base: Long, hypotenuse: Long): Boolean = {
    // (base / 2) ^ 2 + height ^ 2 = hypotenuse ^ 2
    // height ^ 2 = hypotenuse ^ 2 - (base / 2) ^ 2
    // height = sqrt(hypotenuse ^ 2 - (base / 2) ^ 2)
    // area = base * height / 2
    if (hypotenuse % 1000000 == 0 && hypotenuse > base) println(s"Processing $hypotenuse")
    val hypotenuseSquare: Fraction = hypotenuse * hypotenuse
    val baseSquareOver4 = Fraction(base * base, 4)
    val heightSquare = Fraction.minus(hypotenuseSquare, baseSquareOver4, reduce = false)
    if (heightSquare.num % heightSquare.den == 0) {
      MathHelpers.integerSquareRoot((heightSquare.num / heightSquare.den).toLong)
        .map(height => Fraction.fractionIsNumeric.times(height, Fraction(base, 2)))
        .exists(_.den == 1)
    } else if (heightSquare.den == 4) {
      MathHelpers.integerSquareRoot(heightSquare.num.toLong)
        .map(Fraction(_, 2))
        .map(height => Fraction.fractionIsNumeric.times(height, Fraction(base, 2)))
        .exists(_.den == 1)
    } else {
      throw new IllegalStateException(s"Unexpected height square: $heightSquare")
    }
  }
}