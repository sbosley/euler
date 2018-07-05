package sbosley.euler.p51to100.p61to70.problem61

import sbosley.euler.util.math.MathHelpers._

import scala.collection.mutable

object CyclicalFigurateNumbers {

  private val TRIANGULAR = 0
  private val SQUARE = 1
  private val PENTAGONAL = 2
  private val HEXAGONAL = 3
  private val HEPTAGONAL = 4
  private val OCTAGONAL = 5
  case class FigurateNumber(n: Int, `type`: Int)

  private val prefixMap = buildPrefixMap

  def main(args: Array[String]): Unit = {
    val octagonals = fourDigitSet(octagonalNumber[Int]).map(FigurateNumber(_, OCTAGONAL))
    val result = findCyclicSetRecursive(List.empty, Set.empty, octagonals).getOrElse(List.empty).reverse
    println(result)
    println(result.map(_.n).sum)
  }

  private def findCyclicSetRecursive(result: List[FigurateNumber], typesUsed: Set[Int],
                                     numbersToCheck: Set[FigurateNumber]): Option[List[FigurateNumber]] = {
    if (result.size == 6) Some(result)
    else if (numbersToCheck.isEmpty) None
    else numbersToCheck.view.map(figurate => {
      if (result.size == 5) { // Need to check that the cycle is completed on the last element
        if (figurate.n.toString.substring(2) == result.last.n.toString.substring(0, 2)) {
          Some(figurate :: result)
        } else None
      } else {
        val newResult = figurate :: result
        val newTypes = typesUsed + figurate.`type`
        val newNumbersToCheck = prefixMap.getOrElse(figurate.n.toString.substring(2), Set.empty)
          .filter(f => !newResult.contains(f) && !newTypes.contains(f.`type`))
        findCyclicSetRecursive(newResult, newTypes, newNumbersToCheck)
      }
    }).find(_.isDefined).flatten
  }

  private def fourDigitSet(f: Int => Int): Set[Int] = {
    Stream.from(1).map(f).takeWhile(_ < 10000).filter(_ > 999).toSet
  }

  private def buildPrefixMap: Map[String, Set[FigurateNumber]] = {
    val triangulars = fourDigitSet(triangleNumber[Int]) // Size: 96
    val squares = fourDigitSet(squareNumber[Int]) // Size: 68
    val pentagonals = fourDigitSet(pentagonalNumber[Int]) // Size: 56
    val hexagonals = fourDigitSet(hexagonalNumber[Int]) // Size: 48
    val heptagonals = fourDigitSet(heptagonalNumber[Int]) // Size: 43
    val octagonals = fourDigitSet(octagonalNumber[Int]) // Size: 40

    val builder = mutable.Map[String, mutable.Set[FigurateNumber]]()
    addToPrefixMap(triangulars, TRIANGULAR, builder)
    addToPrefixMap(squares, SQUARE, builder)
    addToPrefixMap(pentagonals, PENTAGONAL, builder)
    addToPrefixMap(hexagonals, HEXAGONAL, builder)
    addToPrefixMap(heptagonals, HEPTAGONAL, builder)
    addToPrefixMap(octagonals, OCTAGONAL, builder)
    builder.mapValues(_.toSet).toMap
  }

  private def addToPrefixMap(numbers: Set[Int], `type`: Int,
                             builder: mutable.Map[String, mutable.Set[FigurateNumber]]): mutable.Map[String, mutable.Set[FigurateNumber]] = {
    numbers.foldLeft(builder) { (builder, n) =>
      val prefix = n.toString.substring(0, 2)
      builder.getOrElseUpdate(prefix, mutable.Set.empty) += FigurateNumber(n, `type`)
      builder
    }
  }

}
