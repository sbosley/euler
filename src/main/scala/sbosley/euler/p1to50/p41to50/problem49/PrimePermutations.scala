package sbosley.euler.p1to50.p41to50.problem49

import sbosley.euler.math.Primes

object PrimePermutations {

  private val primes = Primes.primesToMax(10000).filter(_ > 999) // 4 digit primes only
  private val primesSet = primes.toSet

  def main(args: Array[String]): Unit = {
    val primeSeq = primes.map(p => permutedPrimes(p)).flatMap(findEvenlySpacedSeq).filter(_.nonEmpty).toSet
    println(primeSeq)
  }

  private def permutedPrimes(p: Int): List[Int] = {
    val permutedStrings = permute("", p.toString, Set.empty)
    permutedStrings.map(_.toInt).filter(primesSet).toList.sorted
  }

  private def permute(stringSoFar: String, remainingChars: String, permutations: Set[String]): Set[String] = {
    if (remainingChars.isEmpty) permutations + stringSoFar
    else {
      def stringMinusIndex(s: String, index: Int): String = {
        if (index == 0) s.substring(1)
        else if (index == s.length - 1) s.substring(0, index)
        else s.substring(0, index) + s.substring(index + 1)
      }
      permutations ++ remainingChars.zipWithIndex.flatMap({ case (c, index) =>
        permute(stringSoFar + c, stringMinusIndex(remainingChars, index), permutations)
      })
    }
  }

  private def findEvenlySpacedSeq(primes: List[Int]): Set[List[Int]] = {
    primes match {
      case Nil => Set.empty
      case p :: others =>
        others.map(o => {
          val step = o - p
          if (others.contains(o + step)) List(p, p + step, p + step * 2)
          else Nil
        }).filter(_.nonEmpty).toSet ++ findEvenlySpacedSeq(primes.tail)
    }
  }
}
