package sbosley.euler.p451to500.p491to500.problem493

import java.math.MathContext.DECIMAL128

import sbosley.euler.util.math.MathHelpers.choose

object UnderTheRainbow {

  // C(70, 20) possible combos
  // Let C_k be the number of combos with exactly k colors

  def main(args: Array[String]): Unit = {
    // Expected value = 2C_2 + 3C_3 + ... + 7C_7 / C(70, 20)
    assert(C_2 + C_3 + C_4 + C_5 + C_6 + C_7 == TOTAL)
    val numerator = 2 * C_2 + 3 * C_3 + 4 * C_4 + 5 * C_5 + 6 * C_6 + 7 * C_7
    val denominator = TOTAL
    val result = BigDecimal(numerator, DECIMAL128) / BigDecimal(denominator, DECIMAL128)
    println(result)
  }

  val TOTAL: BigInt = choose(70, 20)

  val C_2: BigInt = {
    // # of ways to choose 20 items while leaving out 5 colors
    // i.e. get 10 of one color, 10 of another
    // i.e. get 2 of the 7 colors
    // i.e. C(7, 2)
    choose(7, 5) * choose(20, 20)
  }

  val C_3: BigInt = {
    // Combos that leave out 4 of the 7 colors minus the combos that leave out 5 colors
    choose(7, 4) * choose(30, 20) - C_2
  }

  val C_4: BigInt = {
    // Combos that leave out 3 of the 7 colors minus the combos that leave out 4 or 5 colors
    choose(7, 3) * choose(40, 20) - C_3 - C_2
  }

  val C_5: BigInt = {
    // Combos that leave out 2 of the 7 colors minus the combos that leave out 3, 4, or 5 colors
    choose(7, 2) * choose(50, 20) - C_4 - C_3 - C_2
  }

  val C_6: BigInt = {
    // Combos that leave out one of the 7 colors minus the combos that leave out 2, 3, 4, or 5 colors
    choose(7, 1) * choose(60, 20) - C_5 - C_4 - C_3 - C_2
  }

  val C_7: BigInt = {
    // All combos minus combos that don't choose all the colors
    TOTAL - C_6 - C_5 - C_4 - C_3 - C_2
  }
}
