package sbosley.euler.p1to50.p41to50.problem46

import sbosley.euler.math.Primes

object GoldbachsOtherConjecture {

  private val max = 1000000
  private val primes = Primes.primesToMax(max)
  private val primesSet = primes.toSet
  private val squares = Stream.from(1).map(n => n * n).takeWhile(_ < max).toSet

  def main(args: Array[String]): Unit = {
    val oddComposites = Stream.iterate(9)(_ + 2)
      .filter(c => !primesSet(c))
      .takeWhile(_ < max)

    val example = oddComposites.find(c => {
      !primes.view.filter(_ < c).exists(p => {
        val twiceSquare = c - p
        squares(twiceSquare / 2)
      })
    })
    println(example)
  }
}
