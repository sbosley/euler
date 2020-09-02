package sbosley.euler.p101to150.p111to120.problem115

import sbosley.euler.util.Memoize

object BlockCombinationsII {

  val MIN_LENGTH = 50

  def main(args: Array[String]): Unit = {
    val result = Stream.from(50).find(countTileArrangements(_) + 1 > 1000000) // + 1 for the arrangement of no tiles
    println(result)
  }

  val countTileArrangements: Int => Long = {
    Memoize { countTileArrangementsInternal }
  }

  def countTileArrangementsInternal(remainingSpace: Int): Long = {
    if (remainingSpace <= 1) 0
    else {
      val arrangements = for {
        tileLength <- MIN_LENGTH to remainingSpace
        start <- 0 to remainingSpace - tileLength
      } yield 1 + countTileArrangements(remainingSpace - tileLength - start - 1) // -1 for requiring at least one space between tiles
      arrangements.sum
    }
  }

}
