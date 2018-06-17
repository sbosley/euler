package sbosley.euler.p51to100.p51to60.problem51

import sbosley.euler.math.{MathHelpers, Primes}

import scala.collection.mutable

object PrimeDigitReplacements {

  private val primes = Primes.primesToMax(1000000)

  def main(args: Array[String]): Unit = {
    val digitReplacementFamilies = buildPrimeFamilies(primes)
    val eightPrimeFamilies = digitReplacementFamilies.filter(_._2.size >= 8)
    println(eightPrimeFamilies)
    println(eightPrimeFamilies.head._2.min)
  }

  private def buildPrimeFamilies(primes: List[Int]): Map[String, Set[Int]] = {
    primes.foldLeft(mutable.Map[String, Set[Int]]()) { (builder, prime) =>
      val canonicalRepresentations = getCanonicalRepresentations(prime)
      canonicalRepresentations.foldLeft(builder) { (b, s) =>
        val set = b.getOrElse(s, Set.empty)
        b += s -> (set + prime)
      }
    }.toMap
  }

  private def getCanonicalRepresentations(p: Int): Set[String] = {
    val pString = p.toString
    val charPositions = pString.zipWithIndex.foldLeft(mutable.Map[Char, Set[Int]]()) { case (builder, (char, index)) =>
      val set = builder.getOrElse(char, Set.empty)
      builder += char -> (set + index)
    }.toMap
    charPositions.values.flatMap { positions =>
      val allPositionCombos = MathHelpers.powerset(positions) - Set.empty
      allPositionCombos.map(pos => {
        val charArray = pString.toCharArray
        pos.foldLeft(charArray) { (chars, i) => chars(i) = '*'; chars }
        charArray.mkString
      })
    }.toSet
  }

}
