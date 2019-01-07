package sbosley.euler.p101to150.p101to110.problem104

import sbosley.euler.util.math.MathHelpers

object PandigitalFibonacciEnds {

  private val digits = "123456789"

  def main(args: Array[String]): Unit = {
    val fibStream = MathHelpers.FIBONACCI_STREAM.zipWithIndex
    val result = fibStream.find { case (f, i) => i > 2749 && hasPandigitalEnds(f, i) }
    println(result.get._2)
  }

  private def hasPandigitalEnds(f: BigInt, idx: Int): Boolean = {
    val last9Digits = f % 1000000000L
    val last9String = last9Digits.toString
    last9String.length == 9 && last9String.sorted == digits && f.toString.substring(0, digits.length).sorted == digits
  }

}
