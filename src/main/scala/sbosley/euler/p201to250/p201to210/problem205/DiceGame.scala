package sbosley.euler.p201to250.p201to210.problem205

import sbosley.euler.util.Parallelize

import scala.concurrent.{Await, duration}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object DiceGame {

  def main(args: Array[String]): Unit = {
    val pete = distributionOfPetesRolls
    val colin = distributionOfColinsRolls
    val totalGames = pete.length.toLong * colin.length
    println(s"Pete has ${pete.length} possibilities")
    println(s"Colin has ${colin.length} possibilities")
    println(s"Together, there are $totalGames possible games")
    val peteWinCountFuture = Parallelize(pete) { petesRolls =>
      petesRolls.map(petesRoll => colin.count(_ < petesRoll).toLong).sum
    }.map(_.sum)
    val peteWinCount = Await.result(peteWinCountFuture, Duration(60, duration.SECONDS))
    val result = BigDecimal(peteWinCount) / BigDecimal(totalGames)
    println(s"Probability of pete win: $result")
  }

  private def distributionOfPetesRolls: List[Int] = {
    val diceSides = List(1, 2, 3, 4)
    for {
      d1 <- diceSides
      d2 <- diceSides
      d3 <- diceSides
      d4 <- diceSides
      d5 <- diceSides
      d6 <- diceSides
      d7 <- diceSides
      d8 <- diceSides
      d9 <- diceSides
    } yield {
      d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9
    }
  }

  private def distributionOfColinsRolls: List[Int] = {
    val diceSides = List(1, 2, 3, 4, 5, 6)
    for {
      d1 <- diceSides
      d2 <- diceSides
      d3 <- diceSides
      d4 <- diceSides
      d5 <- diceSides
      d6 <- diceSides
    } yield {
      d1 + d2 + d3 + d4 + d5 + d6
    }
  }

}
