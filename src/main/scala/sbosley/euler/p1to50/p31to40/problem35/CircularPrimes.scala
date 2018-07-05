package sbosley.euler.p1to50.p31to40.problem35


import sbosley.euler.util.string.StringExt._
import sbosley.euler.util.math.Primes

object CircularPrimes {

  private val primes = Primes.primesToMax(1000000).toSet

  def main(args: Array[String]): Unit = {
    val circularPrimesCount = primes.count(isCircularPrime)
    println(circularPrimesCount)
  }

  def isCircularPrime(p: Int): Boolean = {
    val pString = p.toString
    if (p == 2 || pString.length == 1) true
    else {
      val pChars = pString.toSet
      if (pChars('0') || pChars('2') || pChars('4') || pChars('6') || pChars('8')) false
      else allRotations(pString).forall(primes)
    }
  }

  def allRotations(str: String): Seq[Int] = {
    for {
      n <- 1 until str.length
    } yield {
      str.rotate(n).toInt
    }
  }

}
