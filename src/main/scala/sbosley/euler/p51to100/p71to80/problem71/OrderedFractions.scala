package sbosley.euler.p51to100.p71to80.problem71

import sbosley.euler.util.math.Fraction

import scala.annotation.tailrec

object OrderedFractions {

  // Farey sequences (https://en.wikipedia.org/wiki/Farey_sequence)
  // If a/b and c/d are neighbors in a Farey sequence, the first term that appears between them is (a+c)/(b+d), which
  // appears in Farey seq of order b + d

  def main(args: Array[String]): Unit = {
    val leftNeighbor = Fraction(2, 5)
    val rightNeighbor = Fraction(3, 7)
    println(reduceNeighbors(leftNeighbor, rightNeighbor))
  }

  @tailrec
  private def reduceNeighbors(left: Fraction, right: Fraction): Fraction = {
    if (left.den + right.den > 1000000) left
    else reduceNeighbors(Fraction(left.num + right.num, left.den + right.den), right)
  }

}
