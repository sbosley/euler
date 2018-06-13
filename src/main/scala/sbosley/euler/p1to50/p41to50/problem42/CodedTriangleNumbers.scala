package sbosley.euler.p1to50.p41to50.problem42

import sbosley.euler.math.MathHelpers

import scala.io.Source

object CodedTriangleNumbers {

  def main(args: Array[String]): Unit = {
    // Replaced commas with newlines:
    //    sed -i '' 's/,/\
    //    /g' src/main/scala/sbosley/euler/p1to50/p21to30/problem22/p022_names.txt
    val triangleWords = Source.fromFile("src/main/scala/sbosley/euler/p1to50/p41to50/problem42/p042_words.txt")
      .getLines().map(_.replaceAll("\"", ""))
      .map(wordScore)
      .filter(isTriangleNumber)
    println(triangleWords.size)
  }

  private def wordScore(word: String): Int = {
    word.map(_ - 'A' + 1).sum
  }

  private def isTriangleNumber(n: Int): Boolean = {
    val (solutionA, solutionB) = MathHelpers.quadraticFormula(1, 1, -2 * n)
    val positiveSolution = Math.max(solutionA, solutionB)

    val checkN = positiveSolution.toInt
    checkN * (checkN + 1) / 2 == n || ((checkN + 1) * (checkN + 2) / 2) == n
  }

}
