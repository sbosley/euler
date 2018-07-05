package sbosley.euler.p51to100.p61to70.problem65

import sbosley.euler.util.math.PeriodicFractions.ConvergentFraction

object ConvergentsOfE {

  def main(args: Array[String]): Unit = {
    val convergent = ConvergentFraction(2, 1 #:: 2 #:: Stream.from(2).flatMap(i => Seq(1, 1, 2 * i)), None)
      .evalToPrecision(99)
    println(convergent)
    println(convergent.num.toString.map(_.asDigit).sum)
  }

}
