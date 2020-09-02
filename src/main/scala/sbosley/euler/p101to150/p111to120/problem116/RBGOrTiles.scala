package sbosley.euler.p101to150.p111to120.problem116

import sbosley.euler.util.Memoize

object RBGOrTiles {

  val RED_LENGTH = 2
  val GREEN_LENGTH = 3
  val BLUE_LENGTH = 4
  val SPACE = 50

  def main(args: Array[String]): Unit = {
    val r = countTileArrangements(RED_LENGTH, SPACE)
    val g = countTileArrangements(GREEN_LENGTH, SPACE)
    val b = countTileArrangements(BLUE_LENGTH, SPACE)
    println(s"r: $r, g: $g, b: $b, sum: ${r+b+g}")
  }

  val countTileArrangements: ((Int, Int)) => Long = {
    Memoize { x: (Int, Int) =>
      countTileArrangementsInternal(x._1, x._2)
    }
  }

  def countTileArrangementsInternal(tileLength: Int, remainingSpace: Int): Long = {
    if (tileLength > remainingSpace) 0
    else if (tileLength == remainingSpace) 1
    else {
      val arrangements = for {
        start <- 0 to remainingSpace - tileLength
      } yield 1 + countTileArrangements((tileLength, remainingSpace - tileLength - start))
      arrangements.sum
    }
  }

}
