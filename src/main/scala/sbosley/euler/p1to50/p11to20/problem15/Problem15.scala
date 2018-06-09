package sbosley.euler.p1to50.p11to20.problem15

import scala.collection.mutable

object Problem15 {

  private val memoized = mutable.Map[(Int, Int), Long]()

  def main(args: Array[String]): Unit = {
    println(countPaths(20, 20))
  }

  def countPaths(w: Int, h: Int): Long = {
    if (w == 0 || h == 0) 1
    else {
      val cacheKey = (Math.max(w, h), Math.min(w, h))
      if (memoized.contains(cacheKey)) memoized(cacheKey)
      else {
        val result = countPaths(w - 1, h) + countPaths(w, h - 1)
        memoized += cacheKey -> result
        result
      }
    }
  }

}
