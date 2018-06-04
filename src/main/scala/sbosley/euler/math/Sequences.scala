package sbosley.euler.math

import Integral.Implicits._

object Sequences {

  def sumToN[T : Integral](n: T): T = {
    val integral = implicitly[Integral[T]]
    (n * (n + integral.one)) / integral.fromInt(2)
  }

}
