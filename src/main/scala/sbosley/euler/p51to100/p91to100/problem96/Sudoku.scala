package sbosley.euler.p51to100.p91to100.problem96

import scala.collection.mutable
import scala.io.Source

object Sudoku {

  private val allDigits = Set(1, 2, 3, 4, 5, 6, 7, 8, 9)
  case class Cell(row: Int, col: Int, box: Int, value: Either[Int, Set[Int]])
  case class Puzzle(known: List[Cell], unknown: List[Cell])

  def main(args: Array[String]): Unit = {
    val puzzlesFile = Source.fromFile("src/main/scala/sbosley/euler/p51to100/p91to100/problem96/p096_sudoku.txt").getLines
    val puzzles = puzzlesFile.foldLeft(mutable.ListBuffer[mutable.ListBuffer[String]]()) { case (acc, line) =>
      if (line.matches("Grid \\d\\d")) {
        acc += mutable.ListBuffer[String]()
      } else {
        acc.last += line
      }
      acc
    }.map(parseSudoku).toList

    val solvedPuzzles = puzzles.flatMap(solveSudoku)
    if (solvedPuzzles.size != 50) throw new IllegalStateException("Some puzzles were unsolved")
    val result = solvedPuzzles.map(getTopLeftNumber).sum
    println(result)
  }

  private def parseSudoku(rows: Seq[String]): Puzzle = {
    val cells = rows.zipWithIndex.flatMap { case (row, rowIdx) =>
      row.zipWithIndex.map { case (cell, colIdx) =>
        val cellVal = if (cell == '0') Right(allDigits) else Left(cell.asDigit)
        val boxIdx = getBoxIndex(rowIdx, colIdx)
        Cell(rowIdx, colIdx, boxIdx, cellVal)
      }
    }

    val (knownCells, unknownCells) = cells.partition(_.value.isLeft)
    val updatedUnknown = knownCells.foldLeft(unknownCells.toList)(updateUnknownCells).sortBy(_.value.right.get.size)
    Puzzle(knownCells.toList, updatedUnknown)
  }

  private def updateUnknownCells(unknown: List[Cell], known: Cell): List[Cell] = {
    unknown.map { cell =>
      if (cell.row == known.row || cell.col == known.col || cell.box == known.box) {
        cell.copy(value = cell.value.right.map(_ - known.value.left.get))
      } else cell
    }
  }

  private def getBoxIndex(row: Int, col: Int): Int = {
    val boxRow = row / 3
    val boxCol = col / 3
    boxCol + 3 * boxRow
  }

  private def solveSudoku(puzzle: Puzzle): Option[Puzzle] = {
    if (puzzle.unknown.isEmpty) Some(puzzle)
    else {
      val nextUnknown = puzzle.unknown.head
      if (nextUnknown.value.right.get.isEmpty) None
      else {
        val remainingUnknown = puzzle.unknown.tail
        nextUnknown.value.right.get.view.map { tryValue =>
          val newCell = nextUnknown.copy(value = Left(tryValue))
          val newUnknown = updateUnknownCells(remainingUnknown, newCell)
          solveSudoku(puzzle.copy(known = newCell :: puzzle.known, unknown = newUnknown))
        }.find(_.isDefined).flatten
      }
    }
  }

  private def getTopLeftNumber(solvedPuzzle: Puzzle): Int = {
    val cells = solvedPuzzle.known.filter(cell => cell.row == 0 && cell.col <= 2).sortBy(_.col)
    cells(0).value.left.get * 100 + cells(1).value.left.get * 10 + cells(2).value.left.get
  }

}
