package sbosley.euler.p51to100.p81to90.problem83

import sbosley.euler.util.graph.MatrixGraph
import sbosley.euler.util.graph.MatrixGraph.Coord

import scala.io.Source

object PathSum4Ways {

  private val neighborFunc: Coord => Set[Coord] = { case (row, col) => Set(
    (row + 1, col),
    (row, col + 1),
    (row - 1, col),
    (row, col - 1)
  )}

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("src/main/scala/sbosley/euler/p51to100/p81to90/problem83/p083_matrix.txt").getLines.toSeq
    val nodes = lines.map(_.split(",").toSeq.map(_.toLong))

    val matrix = new MatrixGraph(nodes, neighborFunc)
    val result = matrix.dijkstras()
    println(result)
  }
}
