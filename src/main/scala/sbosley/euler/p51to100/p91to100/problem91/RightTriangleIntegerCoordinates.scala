package sbosley.euler.p51to100.p91to100.problem91

object RightTriangleIntegerCoordinates {

  private val MAX = 50

  def main(args: Array[String]): Unit = {
    val coords = (for {
      x1 <- 0 to MAX
      y1 <- 0 to MAX
      x2 <- 0 to MAX
      y2 <- 0 to MAX
    } yield {
      Set((x1, y1), (x2, y2))
    }).toSet

    val result = coords.count { x =>
        val (x1, y1) = x.head
        val (x2, y2) = x.last
        isRightTriangle(x1, y1, x2, y2)
    }
    println(result)
  }

  private def isRightTriangle(x1: Int, y1: Int, x2: Int, y2: Int): Boolean = {
    val p1ToOriginSquared = x1 * x1 + y1 * y1
    val p2ToOriginSquared = x2 * x2 + y2 * y2
    val p1ToP2Squared = (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)

    if (p1ToOriginSquared == 0 || p2ToOriginSquared == 0 || p1ToP2Squared == 0) false
    else {
      val sorted = Seq(p1ToOriginSquared, p2ToOriginSquared, p1ToP2Squared).sorted
      sorted(0) + sorted(1) == sorted(2)
    }
  }

}
