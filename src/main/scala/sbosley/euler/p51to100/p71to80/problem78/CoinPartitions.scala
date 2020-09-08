package sbosley.euler.p51to100.p71to80.problem78

import sbosley.euler.util.Memoize
import sbosley.euler.util.math.MathHelpers

object CoinPartitions {

  def main(args: Array[String]): Unit = {
    (1 to 20).foreach(x => {
      println(countPartitions(x))
    })
    val result = Stream.from(1).find(i => {
      if (i % 200 == 0) println(i)
      countPartitions(i) % 1000000 == 0
    })
    println(result)
  }

  val pentagonalStream: Seq[(Int, Int)] = Stream.from(1).flatMap(
    x => Seq(
      MathHelpers.pentagonalNumber(x),
      MathHelpers.pentagonalNumber(-x)
    )
  ).zipWithIndex

  val countPartitions = Memoize(countPartitionsInternal)

  // From https://en.wikipedia.org/wiki/Partition_function_(number_theory)#Recurrence_relations
  def countPartitionsInternal(n: Int): BigInt = {
    if (n < 0) 0
    else if (n <= 1) 1
    else {
      pentagonalStream.takeWhile(_._1 <= n).map({ case (k, idx) =>
        val p = countPartitions(n - k)
        if ((idx / 2) % 2 == 1) -p
        else p
      }).sum
    }
  }

}
