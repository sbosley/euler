package sbosley.euler.p101to150.p111to120.problem117

import sbosley.euler.util.Memoize

object RGBAndTiles {

  val RED_LENGTH = 2
  val GREEN_LENGTH = 3
  val BLUE_LENGTH = 4
  val ALL_LENGTHS = Seq(RED_LENGTH, GREEN_LENGTH, BLUE_LENGTH)
  val SPACE = 50

  def main(args: Array[String]): Unit = {
    println(countTileArrangements(SPACE) + 1) // + 1 for the arrangement of no tiles
  }

  val countTileArrangements: Int => Long = {
    Memoize { countTileArrangementsInternal }
  }

  def countTileArrangementsInternal(remainingSpace: Int): Long = {
    if (remainingSpace <= 1) 0
    else {
      val arrangements = for {
        tileLength <- ALL_LENGTHS
        start <- 0 to remainingSpace - tileLength
      } yield 1 + countTileArrangements(remainingSpace - tileLength - start)
      arrangements.sum
    }
  }

}
