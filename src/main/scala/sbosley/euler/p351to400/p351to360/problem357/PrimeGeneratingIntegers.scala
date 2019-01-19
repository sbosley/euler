package sbosley.euler.p351to400.p351to360.problem357

import sbosley.euler.util.Parallelize
import sbosley.euler.util.math.Primes

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.{Await, duration}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object PrimeGeneratingIntegers {

  private val max = 100000000
  private val primes = Primes.primesToMax(max + 100)
  private val primesSet = primes.toSet

  def main(args: Array[String]): Unit = {
    val result = Parallelize(1 to max)(_.filter(isPrimeGeneratingInt).map(_.toLong)).map(_.flatten.sum)
    println(Await.result(result, Duration(90L, duration.SECONDS)))
  }

  private def isPrimeGeneratingInt(n: Int): Boolean = {
    primesSet(n + 1) && checkAllDivisorsRecursive(n, n)
  }

  @tailrec
  private def checkAllDivisorsRecursive(n: Int, reducedN: Int, primesLeft: List[Int] = primes, divisorsChecked: mutable.Set[Int] = mutable.Set(1)): Boolean = {
    val p = primesLeft.head
    if (reducedN == 0 || reducedN == 1) true
    else if (reducedN % p == 0) {
      divisorsChecked.forall(d => {
        val newD = d * p
        primesSet(n / newD + newD)
      }) && checkAllDivisorsRecursive(n, reducedN / p, primesLeft, divisorsChecked ++= divisorsChecked.map(_ * p))
    } else checkAllDivisorsRecursive(n, reducedN, primesLeft.tail, divisorsChecked)
  }

}
