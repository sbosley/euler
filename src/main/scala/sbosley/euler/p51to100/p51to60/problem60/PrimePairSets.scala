package sbosley.euler.p51to100.p51to60.problem60

import sbosley.euler.math.{MathHelpers, Primes}

import scala.collection.mutable

object PrimePairSets {

  println("loading primes")
  private val primesList = Primes.primesToMax(100000000).map(_.toString)
  private val allPrimes = primesList.toSet

  println("building maps")
  private val (prefixMap, suffixMap) = buildMaps

  private def buildMaps: (Map[String, Set[String]], Map[String, Set[String]]) = {
    val prefixBuilder = mutable.Map[String, mutable.Set[String]]()
    val suffixBuilder = mutable.Map[String, mutable.Set[String]]()

    val size = primesList.size
    primesList.zipWithIndex.foreach({ case (p, i) =>
      if (i % 10000 == 0) println(s"processing $p at index $i of $size")
      (1 until p.length).foreach(split => {
        val prefix = p.substring(0, split)
        val suffix = p.substring(split)
        if (allPrimes(prefix) && allPrimes(suffix) && allPrimes(suffix + prefix)) {
          prefixBuilder.getOrElseUpdate(prefix, mutable.Set.empty) += suffix
          suffixBuilder.getOrElseUpdate(suffix, mutable.Set.empty) += prefix
        }
      })
    })
    (prefixBuilder.mapValues(_.toSet).toMap, suffixBuilder.mapValues(_.toSet).toMap)
  }

  def main(args: Array[String]): Unit = {
    println(s"starting main with ${allPrimes.size} primes")
    val primeValuePairs = allPrimes.flatMap(toPrimeValuePair)
    println(s"found ${primeValuePairs.size} pairs")
    val primeValueSets = for {
      pair <- primeValuePairs
      complement <- getMinComplement(pair.toSeq)
    } yield {
      pair ++ complement
    }
    val min = primeValueSets.minBy(_.map(BigInt(_)).sum)
    println(min)
    println(min.map(_.toLong).sum)
  }

  private def getMinComplement(pair: Seq[String]): Option[Set[String]] = {
    val (first, second) = (pair.head, pair.last)
    val candidateSet = (prefixMap.getOrElse(first, Set.empty) intersect
      prefixMap.getOrElse(second, Set.empty) intersect
      suffixMap.getOrElse(first, Set.empty) intersect
      suffixMap.getOrElse(second, Set.empty)) -- pair

    val complementPairs = findComplementPairs(candidateSet)
    val complementTrios = findComplementTrios(candidateSet, complementPairs)
    if (complementTrios.isEmpty) None else Some(complementTrios.minBy(_.map(_.toLong).sum))
  }

  private def findComplementPairs(candidateSet: Set[String]): Set[Set[String]] = {
    candidateSet.flatMap(p => {
      candidateSet.find(q => q != p && allPrimes(p + q) && allPrimes(q + p)).map(Set(_, p))
    })
  }

  private def findComplementTrios(candidates: Set[String], complementPairs: Set[Set[String]]): Set[Set[String]] = {
    candidates.flatMap(p => {
      complementPairs.filter(complementsSet(p, _)).map(_ + p)
    })
  }

  private def complementsSet(p: String, others: Set[String]): Boolean = {
    others.forall(q => allPrimes(p + q) && allPrimes(q + p))
  }

  private def toPrimeValuePair(p: String): Set[Set[String]] = {
    val pString = p.toString
    val indices = (1 until pString.length).filter { splitIndex =>
      val prefix = pString.substring(0, splitIndex)
      val suffix = pString.substring(splitIndex)
      !suffix.startsWith("0") && allPrimes(prefix) && allPrimes(suffix) && allPrimes(suffix + prefix)
    }
    indices.map(split => {
      val int1 = pString.substring(0, split)
      val int2 = pString.substring(split)
      Set(int1, int2)
    })(collection.breakOut)
  }
}
