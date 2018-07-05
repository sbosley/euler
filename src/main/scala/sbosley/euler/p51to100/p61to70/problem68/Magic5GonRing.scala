package sbosley.euler.p51to100.p61to70.problem68

object Magic5GonRing {

  private val digits = (1 to 10).toSet

  def main(args: Array[String]): Unit = {
    val solutions = (for {
      x <- 1 to 10
      y <- (1 to 10).filterNot(_ == x)
      z <- (1 to 10).filterNot(n => n == x || n == y)
      startingSeq = Seq(x, y, z)
    } yield {
      find5GonSolutions(startingSeq.sum, startingSeq.last, digits -- startingSeq, Seq(startingSeq))
    }).toSet.flatten.map(normalizeSolution)
    val max16DigitSolution = solutions.map(solutionToBigInt).filter(_.toString.length == 16).max
    println(max16DigitSolution)
  }

  private def find5GonSolutions(targetSum: Int, nextMiddleDigit: Int,
                                remainingDigits: Set[Int], solutionSoFar: Seq[Seq[Int]]): Set[Seq[Seq[Int]]] = {
    if (remainingDigits.size == 1) {
      val originalMiddle = solutionSoFar.head(1)
      if (remainingDigits.head + nextMiddleDigit + originalMiddle == targetSum) {
        Set(solutionSoFar :+ Seq(remainingDigits.head, nextMiddleDigit, originalMiddle))
      } else Set.empty
    } else {
      val remainingPairs = findPairsToSum(targetSum - nextMiddleDigit, remainingDigits)
      remainingPairs.flatMap(nextPair => {
        val min = nextPair.min
        val max = nextPair.max

        val nextSeq1 = Seq(min, nextMiddleDigit, max)
        val nextSeq2 = Seq(max, nextMiddleDigit, min)
        find5GonSolutions(targetSum, nextSeq1.last, remainingDigits -- nextSeq1, solutionSoFar :+ nextSeq1) ++
        find5GonSolutions(targetSum, nextSeq2.last, remainingDigits -- nextSeq2, solutionSoFar :+ nextSeq2)
      })
    }
  }

  private def findPairsToSum(target: Int, candidates: Set[Int]): Set[Set[Int]] = {
    candidates.map(x => {
      if (candidates(target - x)) Set(x, target - x)
      else Set.empty[Int]
    }).filter(_.nonEmpty)
  }

  private def normalizeSolution(solution: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    val startIndex = solution.zipWithIndex.minBy(_._1.head)._2
    solution.slice(startIndex, solution.length) ++ solution.slice(0, startIndex)
  }

  private def solutionToBigInt(solution: Seq[Seq[Int]]): BigInt = {
    val stringSolution = solution.flatMap(_.map(_.toString)).mkString
    BigInt(stringSolution)
  }

}
