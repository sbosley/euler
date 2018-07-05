package sbosley.euler.p51to100.p61to70.problem69

import sbosley.euler.util.math.Fraction
import sbosley.euler.util.math.{MathHelpers, Primes}

object TotientMaximum {

  private val MAX_N = 1000000
  private val primes = Some(Primes.primesToMax(MAX_N + 100))

  def main(args: Array[String]): Unit = {
    val maxTotient = (2 to MAX_N).map(n => Fraction(n, MathHelpers.totient(n, primes))).max
    println(maxTotient)
  }

}
