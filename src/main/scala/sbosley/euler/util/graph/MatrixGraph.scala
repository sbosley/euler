package sbosley.euler.util.graph

object MatrixGraph {
  type Coord = (Int, Int)
}

import MatrixGraph.Coord

import scala.annotation.tailrec

class MatrixGraph(nodes: Seq[Seq[Long]], neighborFunc: Coord => Set[Coord], fakeNodes: Map[MatrixGraph.Coord, Long] = Map.empty) {

  private val indices = for {
    row <- nodes.indices
    col <- nodes.head.indices
  } yield {
    (row, col)
  }

  def dijkstras(start: Coord = (0, 0), dest: Coord = (nodes.length - 1, nodes.head.length - 1)): Long = {

    val tentativeDistances = indices.map[(Coord, Long), Map[Coord, Long]](_ -> Long.MaxValue)(collection.breakOut) ++ fakeNodes.mapValues(_ => Long.MaxValue) + (start -> nodeValue(start))
    val visitedNodes = indices.map[(Coord, Boolean), Map[Coord, Boolean]](_ -> false)(collection.breakOut) ++ fakeNodes.mapValues(_ =>  false)
    dijkstrasRecursive(start, dest, tentativeDistances, visitedNodes)
  }

  private def nodeValue(coord: Coord): Long = {
    fakeNodes.getOrElse(coord, nodes(coord._1)(coord._2))
  }

  @tailrec
  private def dijkstrasRecursive(currentNode: Coord,
                                 destinationNode: Coord,
                                 tentativeDistances: Map[Coord, Long],
                                 visitedNodes: Map[Coord, Boolean]): Long = {

    val currentTentativeDistance = tentativeDistances(currentNode)
    val neighborsToConsider = unvisitedNeighbors(currentNode, tentativeDistances, visitedNodes)
    val newTentativeDistances = neighborsToConsider.foldLeft(tentativeDistances) { case (distances, node) =>
      val newDistance = currentTentativeDistance + nodeValue(node)
      if (newDistance < distances(node)) {
        distances + (node -> newDistance)
      } else distances
    }
    val newVisitedNodes = visitedNodes + (currentNode -> true)
    if (newVisitedNodes(destinationNode)) {
      newTentativeDistances(destinationNode)
    } else {
      val nextCurrNode = newTentativeDistances.filterKeys(k => !newVisitedNodes(k)).minBy(_._2)._1
      dijkstrasRecursive(nextCurrNode, destinationNode, newTentativeDistances, newVisitedNodes)
    }
  }

  private def unvisitedNeighbors(coord: Coord, tentativeDistances: Map[Coord, Long], visitedNodes: Map[Coord, Boolean]): Set[Coord] = {
    neighborFunc(coord).filter { coord => tentativeDistances.contains(coord) && !visitedNodes(coord) }
  }

}
