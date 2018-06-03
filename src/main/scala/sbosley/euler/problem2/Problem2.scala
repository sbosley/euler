package sbosley.euler.problem2

import sbosley.euler.math.Fibonacci

object Problem2 {

  def main(args: Array[String]): Unit = {
    println(sumEvenFibs)
  }

  def sumEvenFibs: BigInt = {
    Fibonacci.stream.filter( _ % 2 == 0).takeWhile(_ <= 4000000).sum
  }

  def primes(max: BigInt): Seq[BigInt] = {

  }
}
