package sbosley.euler.p1to50.p1to10.problem10

import sbosley.euler.math.Primes

object Problem10 {

  def main(args: Array[String]): Unit = {
    val primes = Primes.primesToMax(2000000)
    println(primes.map(_.toLong).sum)
  }

}
