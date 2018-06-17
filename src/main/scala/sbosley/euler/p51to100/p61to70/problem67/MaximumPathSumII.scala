package sbosley.euler.p51to100.p61to70.problem67

import sbosley.euler.p1to50.p11to20.problem18.MaximumPathSum

import scala.io.Source

object MaximumPathSumII {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("src/main/scala/sbosley/euler/p51to100/p61to70/problem67/p067_triangle.txt")
      .getLines.map(_.split(" ").map(_.toInt).toSeq).toSeq.reverse
    val maxPathSum = MaximumPathSum.maxPathSum(lines)
    println(maxPathSum)
  }

}
