package sbosley.euler.p51to100.p91to100.problem97

object LargeNonMersennePrime {

  def main(args: Array[String]): Unit = {
    val lastTenDigits = (BigInt(2).pow(7830457) * 28433 + 1) % BigInt("10000000000")
    println(lastTenDigits)
  }

}
