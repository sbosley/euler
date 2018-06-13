package sbosley.euler.p1to50.p41to50.problem41

import sbosley.euler.math.Primes

object PandigitalPrime {

  private val digitsTo3 = "123".toSet
  private val digitsTo4 = digitsTo3 + '4'
  private val digitsTo5 = digitsTo4 + '5'
  private val digitsTo6 = digitsTo5 + '6'
  private val digitsTo7 = digitsTo6 + '7'

  // Largest pandigital prime can't have more than 7 digits,
  // because all 8 + 9 digit pandigitals are divisible by 3 by virtue of digit sum
  def main(args: Array[String]): Unit = {
    val primes = Primes.primesToMax(7654321)
    val maxPandigital = primes.filter(isPandigital).max
    println(maxPandigital)
  }

  private def isPandigital(n: Int): Boolean = {
    val nStr = n.toString
    val length = nStr.length
    if (length < 3) false
    else {
      val setToCheck = length match {
        case 3 => digitsTo3
        case 4 => digitsTo4
        case 5 => digitsTo5
        case 6 => digitsTo6
        case 7 => digitsTo7
      }
      nStr.length == setToCheck.size && nStr.toSet == setToCheck
    }
  }

}
