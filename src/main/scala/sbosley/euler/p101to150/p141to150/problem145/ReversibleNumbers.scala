package sbosley.euler.p101to150.p141to150.problem145

import scala.annotation.tailrec
import scala.concurrent.{Await, Future, duration}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object ReversibleNumbers {

  private val oddDigits = Set(1L, 3L, 5L, 7L, 9L)
  private val max = 100000000L

  def main(args: Array[String]): Unit = {
    val rangeFutures = getRangeFuturesForCores
    val resultFuture = Future.sequence(rangeFutures).map(_.sum)
    println(Await.result(resultFuture, Duration(60, duration.SECONDS)))
  }

  // Doing this sequentially takes a while, but it's easy to parallelize and runs quickly on 8 cores
  private def getRangeFuturesForCores: Seq[Future[Int]] = {
    val availableCores = Runtime.getRuntime.availableProcessors
    val rangeSize = max / availableCores
    (1 to availableCores).map { n =>
      (n - 1) * rangeSize until n * rangeSize
    }.map { range =>
      println(range)
      Future {
        range.count(isReversible)
      }
    }
  }

  private def isReversible(n: Long): Boolean = {
    n % 10 != 0 && isAllOddDigits(n + reverse(n))
  }

  @tailrec
  private def isAllOddDigits(n: Long): Boolean = {
    if (n == 0) true // Base case
    else {
      val lastDigit = n % 10
      oddDigits(lastDigit) && isAllOddDigits((n - lastDigit) / 10)
    }
  }

  @tailrec
  private def reverse(n: Long, acc: Long = 0): Long = {
    if (n == 0) acc
    else {
      val lastDigit = n % 10
      val newAcc = acc + lastDigit * pow10(digitCount(n) - 1)
      reverse((n - lastDigit) / 10, newAcc)
    }
  }

  private def digitCount(n: Long): Long = {
    math.floor(math.log10(n)).toInt + 1
  }

  private def pow10(x: Long): Long = {
    math.pow(10, x).toLong
  }

}
