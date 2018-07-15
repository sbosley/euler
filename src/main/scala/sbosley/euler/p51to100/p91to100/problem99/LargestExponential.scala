package sbosley.euler.p51to100.p91to100.problem99

import sbosley.euler.util.math.Primes

import scala.io.Source

object LargestExponential {

  private val primes = Primes.primesToMax(1000000)

  case class Exponential(base: Int, pow: Int, index: Int) {
    def toBigInt: BigInt = BigInt(base).pow(pow)
    def reduceFactors(factors: Map[Int, Int]): Exponential = {
      val newBase = factors.foldLeft(base) {
        case (acc, (factor, factorPow)) => acc / math.pow(factor, factorPow).toInt
      }
      copy(base = newBase)
    }
  }

  def main(args: Array[String]): Unit = {
    val exponentsWithIndex = Source.fromFile("src/main/scala/sbosley/euler/p51to100/p91to100/problem99/p099_base_exp.txt")
      .getLines.toSeq.map(pair => {
      val baseAndPow = pair.split(",")
      Exponential(baseAndPow(0).toInt, baseAndPow(1).toInt, 0)
    }).zipWithIndex.map { case (exp, index) => exp.copy(index = index + 1) }

    val result = exponentsWithIndex.foldLeft(exponentsWithIndex.head) {
      case (largestSoFar, next) => maxExponential(largestSoFar, next)
    }
    println(result.index)
  }

  private def maxExponential(e1: Exponential, e2: Exponential): Exponential = {
    val eMaxBase = if (e1.base > e2.base) e1 else e2
    val logBase = eMaxBase.base
    val e1Reduced = reduceByLogBase(e1, logBase)
    val e2Reduced = reduceByLogBase(e2, logBase)
    if (e1Reduced > e2Reduced) e1 else e2
  }

  private def reduceByLogBase(e: Exponential, logBase: Int): Double = {
    if (e.base == logBase) e.pow
    else {
      e.pow * (math.log(e.base) / math.log(logBase))
    }
  }

}
