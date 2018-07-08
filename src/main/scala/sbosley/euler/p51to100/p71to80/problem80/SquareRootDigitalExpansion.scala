package sbosley.euler.p51to100.p71to80.problem80

import sbosley.euler.util.math.NewtonsMethod

object SquareRootDigitalExpansion {

  private val squares = Set(1, 4, 9, 16, 25, 36, 49, 64, 81, 100)

  def main(args: Array[String]): Unit = {
    val result = (1 to 100).filterNot(squares).map(n => {
      new NewtonsMethod(x => x * x - n, x => 2 * x, 120).apply(BigDecimal((n.toDouble + 1) / 2))
    }).map(x => {
      val decimalDigits = x.toString
      decimalDigits.substring(0, 101)
    }).map(_.map(d => if (d == '.') 0 else d.asDigit).sum).sum
    println(result)
  }

}
