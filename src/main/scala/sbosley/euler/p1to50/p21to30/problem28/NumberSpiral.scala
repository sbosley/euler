package sbosley.euler.p1to50.p21to30.problem28

object NumberSpiral {

  //  21 22 23 24 25
  //  20  7  8  9 10
  //  19  6  1  2 11
  //  18  5  4  3 12
  //  17 16 15 14 13
  //
  // In an n x n square:
  // Upper right = n^2
  // Lower right = (n - 2) ^ 2 + (n - 1)
  // Lower left = lower right + (n - 1) = (n - 2) ^ 2 + 2 * (n - 1)
  // Upper left = lower left + (n - 1) = (n - 2) ^ 2 + 3 * (n - 1)
  // n is from 3 to 1001 by 2

  def main(args: Array[String]): Unit = {
    val diagonalSum = (3 to 1001 by 2).map { n =>
      val lowerRight = (n - 2) * (n - 2) + n - 1
      val lowerLeft = lowerRight + n - 1
      val upperLeft = lowerLeft + n - 1
      val upperRight = upperLeft + n - 1
      lowerRight + lowerLeft + upperRight + upperLeft
    }.sum + 1
    println(diagonalSum)
  }

}
