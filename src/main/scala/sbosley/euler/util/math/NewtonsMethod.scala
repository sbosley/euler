package sbosley.euler.util.math

import java.math.MathContext

import scala.annotation.tailrec

class NewtonsMethod(f: Function[BigDecimal, BigDecimal],
                    `f'`: Function[BigDecimal, BigDecimal],
                    precisionDigits: Int) {

  private val mc = new MathContext(precisionDigits + 20)
  private lazy val diffThreshold = BigDecimal(s"0.${Array.fill(precisionDigits)('0').mkString}1")

  // x_n+1 = x_n - f(x_n)/f'(x_n)

  def apply(guess: BigDecimal): BigDecimal = {
    applyIteratively(guess(mc))
  }

  @tailrec
  private def applyIteratively(guess: BigDecimal): BigDecimal = {
    val nextGuess = guess - (f(guess) / `f'`(guess))
    if (f(nextGuess) == 0 || withinPrecision(guess, nextGuess)) nextGuess
    else applyIteratively(nextGuess)
  }

  private def withinPrecision(guess: BigDecimal, nextGuess: BigDecimal): Boolean = {
    guess.precision > precisionDigits && nextGuess.precision > precisionDigits && (nextGuess - guess).abs < diffThreshold
  }

}
