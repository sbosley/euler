package sbosley.euler.p1to20.problem14

import scala.collection.mutable

object Problem14 {

  private val memoizedLengths = mutable.Map[Long, Long]()
  memoizedLengths += 1L -> 1L

  def main(args: Array[String]): Unit = {
    println((1L to 1000000L).maxBy(collatzLength))
  }

  private def collatzLength(n: Long): Long = {
    if (memoizedLengths.contains(n)) {
      memoizedLengths(n)
    } else {
      val length = if (n % 2 == 0) {
        1 + collatzLength(n / 2)
      } else {
        1 + collatzLength(3 * n + 1)
      }
      memoizedLengths += n -> length
      length
    }
  }

}
