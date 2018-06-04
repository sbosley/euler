package sbosley.euler.problem10

import sbosley.euler.math.Primes

object Problem10 {

  def main(args: Array[String]): Unit = {
    val primes = Primes.primesToMax(2000000)
    println(primes.map(_.toLong).sum)
  }

}
