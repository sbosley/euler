package sbosley.euler.p51to100.p71to80.problem72

import sbosley.euler.util.math.{MathHelpers, Primes}

object CountingFractions {

  // Farey sequences (https://en.wikipedia.org/wiki/Farey_sequence)
  // Traditional Farey sequences include 0 and 1
  // The Farey sequence of order n contains all of the members of the Farey sequences of lower orders. In particular
  // Fn contains all of the members of Fn−1 and also contains an additional fraction for each number that is less than
  // n and coprime to n.

  private val MAX_N = 1000000
  private val primes = Primes.primesToMax(MAX_N + 100)

  def main(args: Array[String]): Unit = {
    val seqSize = (1 to MAX_N).map(n => BigInt(MathHelpers.totient(n, Some(primes)))).sum - 1
    println(seqSize)
  }
}
