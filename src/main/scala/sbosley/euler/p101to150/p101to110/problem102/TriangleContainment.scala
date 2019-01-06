package sbosley.euler.p101to150.p101to110.problem102

import sbosley.euler.util.math.Fraction

import scala.io.Source

object TriangleContainment {

  case class Point(x: Int, y: Int)

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("src/main/scala/sbosley/euler/p101to150/p101to110/problem102/p102_triangles.txt").getLines.toSeq
    val triangles = lines.map(parseTriangle)
    val result = triangles.count({ case (a, b, c) =>
      containsOrigin(a, b, c)
    })
    println(result)
  }

  private def parseTriangle(s: String): (Point, Point, Point) = {
    val coords = s.split(",")
    val a = Point(coords(0).toInt, coords(1).toInt)
    val b = Point(coords(2).toInt, coords(3).toInt)
    val c = Point(coords(4).toInt, coords(5).toInt)
    (a, b, c)
  }

  private def containsOrigin(a: Point, b: Point, c: Point): Boolean = {
    println(s"Checking ($a, $b, $c)")
    // Find how many of the three segments intersect the positive x axis
    // If odd, then origin is in triangle
    val traceIntersectCount =
      segmentIntersectsPositiveXAxis(a, b) +
      segmentIntersectsPositiveXAxis(b, c) +
      segmentIntersectsPositiveXAxis(a, c)
    traceIntersectCount % 2 != 0
  }

  // Returns 1 if true, 0 if false
  private def segmentIntersectsPositiveXAxis(p1: Point, p2: Point): Int = {
    val yMin = math.min(p1.y, p2.y)
    val yMax = math.max(p1.y, p2.y)

    val result =
      if (yMax < 0 || yMin > 0 || p1.y == p2.y) false
      else if (p1.x == p2.x) { // Slope is infinity
        p1.x >= 0
      } else {
        import Fraction.fractionIsNumeric
        // plot segment as y = mx + b
        val m = Fraction(p2.y - p1.y, p2.x - p1.x).reduce // Slope
        val b = fractionIsNumeric.minus(p1.y, m * p1.x)
        val xValForYIntercept = fractionIsNumeric.times(b * -1, m.invert)
        fractionIsNumeric.compare(xValForYIntercept, 0) >= 0
      }

    if (result) 1 else 0
  }
}
