package sbosley.euler.util

import scala.collection.mutable

object Memoize {

  def apply[T, R](f: T => R): T => R = {
    new (T => R) {
      private val memoizedResults = mutable.Map.empty[T, R]
      override def apply(k: T): R = memoizedResults.getOrElseUpdate(k, f(k))
    }
  }

}
