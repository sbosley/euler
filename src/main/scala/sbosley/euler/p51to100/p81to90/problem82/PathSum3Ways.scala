package sbosley.euler.p51to100.p81to90.problem82

import sbosley.euler.util.graph.MatrixGraph
import sbosley.euler.util.graph.MatrixGraph.Coord

import scala.io.Source

object PathSum3Ways {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("src/main/scala/sbosley/euler/p51to100/p81to90/problem82/p082_matrix.txt").getLines.toSeq
    val nodes = lines.map(_.split(",").toSeq.map(_.toLong))

    val fakeStart = (-1, -1)
    val fakeDest = (Int.MaxValue, Int.MaxValue)
    val fakeNodes = Map(
      fakeStart -> 0L, // Fake start
      fakeDest -> 0L // Fake sink
    )
    val matrix = new MatrixGraph(nodes, neighborFunc(nodes), fakeNodes)
    val result = matrix.dijkstras(fakeStart, fakeDest)
    println(result)
  }

  private def neighborFunc(nodes: Seq[Seq[Long]]): Coord => Set[Coord] = {
    {
      case (-1, -1) => nodes.indices.map[Coord, Set[Coord]](row => (row, 0))(collection.breakOut) // Fake start node connects to all real start nodes
      case (row, col) if col == nodes.head.length - 1 => Set( // In last column, can move up, down, or to sink
        (row + 1, nodes.head.length - 1),
        (row - 1, nodes.head.length - 1),
        (Int.MaxValue, Int.MaxValue)
      )
      case (row, col) => Set(
        (row + 1, col),
        (row - 1, col),
        (row, col + 1)
      )
    }
  }
}
