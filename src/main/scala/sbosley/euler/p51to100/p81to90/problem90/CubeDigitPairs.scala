package sbosley.euler.p51to100.p81to90.problem90

object CubeDigitPairs {

  private val ARRANGEMENT_SIZE = 6
  private val DIGITS = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

  def main(args: Array[String]): Unit = {
    val result = allCubeArrangements.count(diePairFormsAllSquares)
    println(result)
  }

  private def allCubeArrangements: Set[Set[Set[Int]]] = {
    val allArrangements = allArrangementsFromDigits(DIGITS, ARRANGEMENT_SIZE)
    for {
      die1 <- allArrangements
      die2 <- allArrangements
    } yield {
      Set(die1, die2)
    }
  }

  private def allArrangementsFromDigits(digits: List[Int], size: Int): Set[Set[Int]] = {
    if (size == 0 || digits.size < size) Set.empty
    else if (size == 1) digits.map(Set(_)).toSet
    else if (digits.size == size) Set(digits.toSet)
    else allArrangementsFromDigits(digits.tail, size - 1).map(_ + digits.head) ++
      allArrangementsFromDigits(digits.tail, size)
  }

  private def diePairFormsAllSquares(diePair: Set[Set[Int]]): Boolean = {
    val die1 = diePair.head
    val die2 = diePair.last
    // Check for 01, 04, 09, 16, 25, 36, 49, 64, 81
    ((die1(0) && die2(1)) || (die2(0) && die1(1))) && // 01
      ((die1(0) && die2(4)) || (die2(0) && die1(4))) && // 04
      ((die1(0) && (die2(6) || die2(9))) || (die2(0) && (die1(6) || die1(9)))) && // 09
      ((die1(1) && (die2(6) || die2(9))) || (die2(1) && (die1(6) || die1(9)))) && // 16
      ((die1(2) && die2(5)) || (die2(2) && die1(5))) && // 25
      ((die1(3) && (die2(6) || die2(9))) || (die2(3) && (die1(6) || die1(9)))) && // 36
      ((die1(4) && (die2(6) || die2(9))) || (die2(4) && (die1(6) || die1(9)))) && // 49 and 64
      ((die1(8) && die2(1)) || (die2(8) && die1(1))) // 81
  }

}
