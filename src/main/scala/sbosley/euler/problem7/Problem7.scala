package sbosley.euler.problem7

import sbosley.euler.math.Primes

object Problem7 {

  def main(args: Array[String]): Unit = {
    println(Primes.primes[Long].apply(10000))
  }

}
